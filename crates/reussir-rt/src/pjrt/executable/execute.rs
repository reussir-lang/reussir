//! Synchronous invocation borrowing the executable and all input buffers.

use std::ptr;

use super::{Error, Executable, Result, call, non_null};
use crate::pjrt::{api::Event, sys::*};

impl Executable {
    /// Inputs must be live buffers belonging to this executable's client, and
    /// remain borrowed until execution completes. Outputs transfer ownership.
    pub(in crate::pjrt) unsafe fn execute(
        &self,
        inputs: &[*mut PJRT_Buffer],
        num_outputs: usize,
    ) -> Result<Vec<*mut PJRT_Buffer>> {
        let api = self.owner.api;
        let devices = call!(
            api,
            PJRT_LoadedExecutable_AddressableDevices {
                executable: self.as_ptr()
            }
        )?;
        if devices.num_addressable_devices != 1 {
            return Err(Error::local(
                "execution requires exactly one addressable device",
            ));
        }
        let executable = call!(
            api,
            PJRT_LoadedExecutable_GetExecutable {
                loaded_executable: self.as_ptr()
            }
        )?
        .executable;
        non_null(executable, "executable")?;
        let count = call!(api, PJRT_Executable_NumOutputs { executable });
        let cleanup = call!(api, PJRT_Executable_Destroy { executable });
        let count = count?.num_outputs;
        cleanup?;
        if count != num_outputs {
            return Err(Error::local(format!(
                "executable returns {count} outputs, but invocation provides {num_outputs} slots"
            )));
        }
        if inputs.iter().any(|input| input.is_null()) {
            return Err(Error::local("execution received a null input buffer"));
        }
        let num_inputs =
            i64::try_from(inputs.len()).map_err(|_| Error::local("too many execution inputs"))?;
        // Borrowed inputs may be reused or appear more than once. Never allow
        // the backend to donate their storage to this invocation's results.
        let non_donatable: Vec<i64> = (0..num_inputs).collect();
        let mut options = PJRT_ExecuteOptions {
            struct_size: PJRT_ExecuteOptions_STRUCT_SIZE as usize,
            non_donatable_input_indices: non_donatable.as_ptr(),
            num_non_donatable_input_indices: non_donatable.len(),
            ..Default::default()
        };
        let mut outputs = vec![ptr::null_mut(); num_outputs];
        let arguments = inputs.as_ptr();
        let results = outputs.as_mut_ptr();
        let mut complete = ptr::null_mut();
        let launched = call!(
            api,
            PJRT_LoadedExecutable_Execute {
                executable: self.as_ptr(),
                options: &mut options,
                argument_lists: &arguments,
                num_devices: 1,
                num_args: inputs.len(),
                output_lists: &results,
                device_complete_events: &mut complete,
            }
        );
        // PJRT leaves completion events unpopulated on launch errors. A
        // successful launch must be awaited even if it produces no outputs.
        let ready = launched.and_then(|_| Event::new(complete)?.wait(api));
        let ready = ready.and_then(|_| {
            if outputs.iter().any(|output| output.is_null()) {
                Err(Error::local("PjRt returned null execution output"))
            } else {
                Ok(())
            }
        });
        if let Err(error) = ready {
            // No output ownership has reached the caller. Release any handles
            // returned by the backend, preserving the original failure.
            for buffer in outputs.into_iter().filter(|buffer| !buffer.is_null()) {
                if let Err(cleanup) = call!(api, PJRT_Buffer_Destroy { buffer }) {
                    tracing::warn!(%cleanup, "PJRT failed-execution output cleanup failed");
                }
            }
            return Err(error);
        }
        Ok(outputs)
    }
}

#[cfg(test)]
mod tests;
