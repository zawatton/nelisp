#!/usr/bin/env python3
"""Independent screenshot checks; blank/two-tone fixtures must fail."""
import json
from pathlib import Path
import sys
import subprocess

def inspect(path):
    dims = subprocess.check_output(['identify','-format','%w %h',str(path)],text=True)
    assert dims == '800 500', dims
    # Rows: ASCII then Japanese, with whitespace margins. Gray AA must survive.
    result = {}
    for name, rect in [('ascii', (20, 35, 120, 70)), ('japanese', (120, 35, 205, 70))]:
        x,y,right,bottom = rect
        raw = subprocess.check_output(['convert',str(path),'-crop',f'{right-x}x{bottom-y}+{x}+{y}',
                                       '-depth','8','rgb:-'])
        assert len(raw) == (right-x)*(bottom-y)*3, len(raw)
        colors = {tuple(raw[i:i+3]) for i in range(0,len(raw),3)}
        gray = {r for r,g,b in colors if r == g == b}
        intermediate = {x for x in gray if 0 < x < 255}
        assert len(intermediate) >= 8, (name, len(intermediate))
        assert 0 in gray and 255 in gray, (name, gray)
        result[name] = dict(gray_levels=len(gray), intermediate_levels=len(intermediate))
    return result

if __name__ == '__main__':
    path = Path(sys.argv[1])
    result = inspect(path)
    out = path.with_suffix('.pixels.json')
    out.write_text(json.dumps(result, indent=2)+'\n')
    print('PIXELS-PASS', json.dumps(result))
