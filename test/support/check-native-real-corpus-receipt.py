"""Exercise the exact F3 verdict against intact or deliberately broken receipts."""
import importlib.util
import json
import os
import re
from pathlib import Path

spec = importlib.util.spec_from_file_location('f3', Path(__file__).with_name('run-native-real-corpus.py'))
runner = importlib.util.module_from_spec(spec)
spec.loader.exec_module(runner)
receipt_path = Path(os.environ['F3_RECEIPT'])
receipt = json.loads(receipt_path.read_text())
output = receipt_path.with_suffix('.out').read_text()
errors = receipt_path.with_suffix('.err').read_text()
mutation = os.environ.get('F3_MUTATION', 'intact')
original = (output, errors, receipt['rc'], receipt['seconds'], list(receipt['names']))
if mutation == 'missing-name':
    output = '\n'.join(line for line in output.splitlines() if 'F3-FUNCTION-PASS' not in line)
elif mutation == 'timeout':
    receipt['seconds'] = 300.1
elif mutation == 'exit':
    receipt['rc'] = 1
elif mutation == 'stderr':
    errors = 'unexpected runtime error'
elif mutation == 'validation':
    output = output.replace('validations=0', 'validations=1')
elif mutation == 'duplicate':
    output += next(line for line in output.splitlines() if line.startswith('F3-FUNCTION-PASS')) + '\n'
elif mutation == 'no-machine-entry':
    output = re.sub(r'native-entries=[1-9][0-9]*', 'native-entries=0', output)
elif mutation == 'entry-count':
    output = re.sub(r'native-entries=([1-9][0-9]*)', lambda match: 'native-entries=' + str(int(match[1]) + 1), output)
elif mutation == 'case-count':
    output = re.sub(r'cases=([1-9][0-9]*) native-entries=([1-9][0-9]*)', lambda match: 'cases={0} native-entries={0}'.format(int(match[1]) + 1), output)
elif mutation == 'empty':
    receipt['names'] = []; output = ''
elif mutation != 'intact':
    raise SystemExit('Unknown mutation')
if mutation != 'intact' and original == (output, errors, receipt['rc'], receipt['seconds'], receipt['names']):
    raise SystemExit('Mutation did not change the receipt')
metadata = json.loads((receipt_path.parents[2] / 'corpus.json').read_text())
expected = {row['name']: row['cases'] for row in metadata}
ok, names, failures = runner.phase_verdict(receipt['backend'], receipt['phase'], receipt['names'], receipt['rc'], receipt['seconds'], output, errors, expected)
print('F3-RECEIPT checked={} mutation={} passed={}'.format(len(receipt['names']), mutation, ok))
raise SystemExit(0 if ok else 1)
