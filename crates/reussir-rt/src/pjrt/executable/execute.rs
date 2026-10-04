//! Synchronous invocation consuming an executable reference and borrowing inputs.

use std::{ptr, sync::Arc};

use super::{Error, Executable, Result, call, non_null};
use crate::pjrt::{api::Event, sys::*};

impl Executable {
    /// Each input points to one target-array descriptor's allocation-handle
    /// field, containing one buffer per addressable device in executable order.
    /// Consumes the executable reference after all devices finish. The descriptors
    /// and their buffers remain borrowed until then.
    /// Results are grouped by logical array and transfer buffer ownership.
    pub(in crate::pjrt) unsafe fn execute(
        self: Arc<Self>,
        inputs: &[*const *mut PJRT_Buffer],
        num_outputs: usize,
    ) -> Result<Vec<Vec<*mut PJRT_Buffer>>> {
        let api = self.owner.api;
        let devices = call!(
            api,
            PJRT_LoadedExecutable_AddressableDevices {
                executable: self.as_ptr()
            }
        )?;
        let num_devices = devices.num_addressable_devices;
        if num_devices == 0 {
            return Err(Error::local("executable has no addressable devices"));
        }
        let executable = call!(
            api,
            PJRT_LoadedExecutable_GetExecutable {
                loaded_executable: self.as_ptr()
            }
        )?
        .executable;
        non_null(executable, "executable")?;
        // Defer error propagation to always destroy the handle and preserve the count error if both calls fail.
        let count = call!(api, PJRT_Executable_NumOutputs { executable });
        let cleanup = call!(api, PJRT_Executable_Destroy { executable });
        let count = count?.num_outputs;
        cleanup?;
        if count != num_outputs {
            return Err(Error::local(format!(
                "executable returns {count} outputs, but invocation provides {num_outputs} slots"
            )));
        }
        let num_inputs = inputs.len();
        let mut device_inputs = vec![Vec::with_capacity(num_inputs); num_devices];
        for &array in inputs {
            non_null(array.cast_mut(), "input array descriptor")?;
            for (device, row) in device_inputs.iter_mut().enumerate() {
                let buffer = unsafe { *array.add(device) };
                non_null(buffer, "input array shard")?;
                row.push(buffer);
            }
        }
        let non_donatable_count =
            i64::try_from(num_inputs).map_err(|_| Error::local("too many execution inputs"))?;
        // Borrowed inputs may be reused or appear more than once. Never allow
        // the backend to donate their storage to this invocation's results.
        let non_donatable: Vec<i64> = (0..non_donatable_count).collect();
        let mut options = PJRT_ExecuteOptions {
            struct_size: PJRT_ExecuteOptions_STRUCT_SIZE as usize,
            non_donatable_input_indices: non_donatable.as_ptr(),
            num_non_donatable_input_indices: non_donatable.len(),
            ..Default::default()
        };
        let mut outputs = vec![vec![ptr::null_mut(); num_outputs]; num_devices];
        let arguments: Vec<_> = device_inputs.iter().map(|row| row.as_ptr()).collect();
        let results: Vec<_> = outputs.iter_mut().map(|row| row.as_mut_ptr()).collect();
        let mut complete = vec![ptr::null_mut(); num_devices];
        let launched = call!(
            api,
            PJRT_LoadedExecutable_Execute {
                executable: self.as_ptr(),
                options: &mut options,
                argument_lists: arguments.as_ptr(),
                num_devices,
                num_args: num_inputs,
                output_lists: results.as_ptr(),
                device_complete_events: complete.as_mut_ptr(),
            }
        );
        // PJRT leaves completion events unpopulated on launch errors. A
        // successful launch must be awaited even if it produces no outputs.
        let mut ready = launched.map(|_| ());
        if ready.is_ok() {
            for event in complete {
                // Drain every device even when an earlier one failed: inputs
                // and the executable stay borrowed until all devices finish.
                let waited = Event::new(event).and_then(|event| event.wait(api));
                ready = ready.and(waited);
            }
        }
        let ready = ready.and_then(|_| {
            if outputs.iter().flatten().any(|output| output.is_null()) {
                Err(Error::local("PjRt returned null execution output"))
            } else {
                Ok(())
            }
        });
        if let Err(error) = ready {
            // No output ownership has reached the caller. Release any handles
            // returned by the backend, preserving the original failure.
            for buffer in outputs
                .into_iter()
                .flatten()
                .filter(|buffer| !buffer.is_null())
            {
                if let Err(cleanup) = call!(api, PJRT_Buffer_Destroy { buffer }) {
                    tracing::warn!(%cleanup, "PJRT failed-execution output cleanup failed");
                }
            }
            return Err(error);
        }
        // Return one row per logical result so the FFI writes directly into
        // each destination array descriptor's allocation-handle field.
        Ok((0..num_outputs)
            .map(|result| outputs.iter().map(|row| row[result]).collect())
            .collect())
    }
}

#[cfg(test)]
mod tests;
