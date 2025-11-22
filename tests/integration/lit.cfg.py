import lit.formats
import sys

config.name = 'Reussir'
config.test_format = lit.formats.ShTest(True)

config.suffixes = ['.mlir']

config.test_source_root = os.path.dirname(__file__)
config.test_exec_root = os.path.join(config.test_output_root, 'test')

config.substitutions.append((r'%reussir-opt',
                             os.path.join(config.binary_path, 'reussir-opt')))
config.substitutions.append((r'%reussir-translate',
                             os.path.join(config.binary_path, 'reussir-translate')))

# Add C compiler substitution using CMake's C compiler
config.substitutions.append((r'%cc', config.cc_path))

config.substitutions.append((r'%FileCheck', config.filecheck_path))
config.substitutions.append((r'%not', config.not_path))
config.substitutions.append((r'%opt', config.opt_path))
config.substitutions.append((r'%library_path', config.library_path))
config.substitutions.append((r'%llc', config.llc_path))
config.substitutions.append((r'%extra_sys_libs', config.extra_sys_libs))
config.substitutions.append((r'%lli', config.lli_path))

# TODO: should we support macos?
if sys.platform == 'windows':
    config.substitutions.append((r'%reussir_rt', 'reussir_rt.dll'))
else:
    config.substitutions.append((r'%reussir_rt', 'libreussir_rt.so'))
