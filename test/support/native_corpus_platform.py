"""Portable corpus process/path boundary shared with the Windows F1 runner."""
import importlib.util
import os
from pathlib import Path
import time

_spec = importlib.util.spec_from_file_location(
    'windows_native_f1', Path(__file__).with_name('run-windows-native-f1.py'))
_f1 = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_f1)
ROOT = _f1.ROOT
WINDOWS = False
WINE = False
PROCESS_DEADLINE = 290


def add_arguments(parser):
    parser.add_argument('--windows', action='store_true', help='Windows reader; requires its full cold image.')
    parser.add_argument('--wine', action='store_true', help='Coordinator evidence only; convert reader paths through winepath.')
    parser.add_argument('--cold', action='store_true', help='Require the binary.cold image (Windows always requires it).')
    parser.add_argument('--run-deadline', type=int, default=1800,
                        help='Overall wall budget in seconds; default 1800 locally, CI may use up to 19800.')


def configure(args, parser):
    global WINDOWS, WINE, PROCESS_DEADLINE
    WINDOWS = os.name == 'nt' or args.windows or args.wine
    WINE = args.wine
    if WINE:
        os.environ.setdefault('WINEPREFIX', str(Path.home() / '.cache/wine-nelisp'))
        os.environ['WINEDEBUG'] = '-all'
    if WINDOWS and os.name != 'nt' and not WINE:
        parser.error('Windows reader on POSIX requires --wine; host evidence cannot qualify Windows')
    if not 1 <= args.run_deadline <= 19800:
        parser.error('run deadline must be 1..19800 seconds')
    if WINDOWS and (getattr(args, 'both', False) or args.backend == 'gccjit'):
        parser.error('Windows does not support gccjit/--both')
    PROCESS_DEADLINE = min(1800, int(os.environ.get('NELISP_WINDOWS_NATIVE_DEADLINE', '900'))) if WINDOWS else 290
    if PROCESS_DEADLINE < 1:
        parser.error('process deadline must be positive')
    _f1.RUN_DEADLINE = time.monotonic() + args.run_deadline
    # Refuse before GNU fixture generation or copying a reader/image.
    binaries = [args.static if hasattr(args, 'static') else args.binary]
    if getattr(args, 'both', False): binaries.append(args.dynamic)
    for binary in binaries:
        if args.cold or WINDOWS:
            image = Path(str((ROOT / binary).resolve()) + '.cold')
            if not image.is_file():
                parser.error('full compiler cold image required: ' + str(image))


def run_process(command, env, directory, phase, deadline=None):
    if _f1.RUN_DEADLINE is not None and time.monotonic() >= _f1.RUN_DEADLINE:
        (directory / (phase + '.out')).write_bytes(b'')
        (directory / (phase + '.err')).write_bytes(b'Corpus wall deadline exhausted; no process launched\n')
        return 124, 0, '', 'Corpus wall deadline exhausted; no process launched\n'
    receipt = _f1.run(command, env, directory, phase, deadline=deadline or PROCESS_DEADLINE)
    output = (directory / (phase + '.out')).read_bytes().decode('utf-8', errors='replace').replace('\r\n', '\n')
    errors = (directory / (phase + '.err')).read_bytes().decode('utf-8', errors='replace').replace('\r\n', '\n')
    return receipt['rc'], receipt['seconds'], output, errors


def reader_command(binary, cold, driver):
    return _f1.reader_command(binary, cold if cold.is_file() else None, driver, wine=WINE)


def reader_environment(env, variables):
    result = dict(env)
    if WINE:
        for variable in variables:
            if variable in result:
                result[variable] = _f1.wine_path(result[variable])
    return result


def cache_file(path, cache):
    """Map reader-reported paths back to the host, confined to the cohort cache."""
    if WINE:
        prefix = _f1.wine_path(cache).replace('\\', '/').rstrip('/') + '/'
        normalized = path.replace('\\', '/')
        if not normalized.lower().startswith(prefix.lower()):
            raise ValueError('Reader artifact escaped its cache')
        relative = normalized[len(prefix):]
        if any(part in ('', '.', '..') for part in relative.split('/')) or ':' in relative:
            raise ValueError('Reader artifact path refused')
        artifact = cache / relative
    else:
        artifact = Path(path)
    artifact.relative_to(cache)
    return artifact


def load_average():
    operation = getattr(os, 'getloadavg', None)
    return operation() if operation else None


def load_text(value):
    return 'unavailable' if value is None else f'{value[0]:.2f}'


def identity():
    return dict(platform='wine' if WINE else ('windows' if WINDOWS else 'posix'),
                process_deadline=PROCESS_DEADLINE,
                platform_runner_sha256=_f1.digest(Path(__file__)),
                process_runner_sha256=_f1.digest(Path(_f1.__file__)))


def create_cache(path):
    """Windows reader owns protected DACL creation; POSIX uses private modes."""
    if not WINDOWS:
        path.mkdir(mode=0o700, parents=True, exist_ok=True)
    return path
