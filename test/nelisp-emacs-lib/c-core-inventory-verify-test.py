#!/usr/bin/env python3
"""Negative controls for the C-core census/area/probe inventory verifier."""
import importlib.util
from collections import Counter
from pathlib import Path
import tempfile
import unittest


MODULE = Path(__file__).with_name("c-core-inventory-verify.py")
SPEC = importlib.util.spec_from_file_location("c_core_inventory_verify", MODULE)
VERIFY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(VERIFY)


class InventoryVerifierTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="c-core-inventory-")
        self.root = Path(self.temp.name)
        self.census = self.root / "census.tsv"
        self.areas = self.root / "areas.tsv"
        self.probes = self.root / "probes"
        self.probes.mkdir()
        self.names = [f"primitive-{i:04d}" for i in range(1460)]
        self.write_fixture()

    def tearDown(self):
        self.temp.cleanup()

    def write_fixture(self, names=None, area_rows=None, probe_text=None):
        names = self.names if names is None else names
        self.census.write_text("# census\n" + "".join(
            f"{name}\tinterpreted\t1\t0\t0\n" for name in names), encoding="utf-8")
        area_rows = [(name, "other") for name in names] if area_rows is None else area_rows
        self.areas.write_text("".join("\t".join(row) + "\n" for row in area_rows), encoding="utf-8")
        if probe_text is None:
            probe_text = "".join(f"({name} t)\n" for name in names)
        (self.probes / "fixture.el").write_text(probe_text, encoding="utf-8")

    def verify(self):
        return VERIFY.verify(self.census, self.areas, self.probes)

    def test_complete_inventory_passes(self):
        result = self.verify()
        self.assertEqual((result["census"], result["areas"], result["unique_probe_names"]),
                         (1460, 1460, 1460))

    def test_self_consistent_truncated_inventory_fails(self):
        names = ["short-a", "short-b", "short-c"]
        self.write_fixture(names=names)
        with self.assertRaisesRegex(ValueError, '"expected_census": 1460'):
            self.verify()

    def test_invalid_census_state_fails(self):
        self.write_fixture()
        self.census.write_text(self.census.read_text(encoding="utf-8").replace(
            "primitive-0000\tinterpreted", "primitive-0000\tunknown", 1), encoding="utf-8")
        with self.assertRaisesRegex(ValueError, "census: malformed rows"):
            self.verify()

    def test_unknown_area_category_fails(self):
        self.write_fixture(area_rows=[(self.names[0], "ignored")]
                           + [(name, "other") for name in self.names[1:]])
        with self.assertRaisesRegex(ValueError, "areas: malformed rows"):
            self.verify()

    def test_missing_ownership_fails_closed(self):
        self.write_fixture(area_rows=[(name, "other") for name in self.names[:-1]])
        with self.assertRaisesRegex(ValueError, '"missing_areas": 1'):
            self.verify()

    def test_duplicate_unknown_and_malformed_area_rows_fail(self):
        for rows, expected in (
            ([(self.names[0], "other"), (self.names[0], "other")], "duplicate names"),
            ([(self.names[0], "other"), ("ghost", "other")], '"unknown_areas": 1'),
            ([(self.names[0], "other"), ("bad", "row", "extra")], "malformed rows"),
        ):
            self.write_fixture(area_rows=rows)
            with self.subTest(expected=expected), self.assertRaisesRegex(ValueError, expected):
                self.verify()

    def test_missing_or_unknown_probe_fails_closed(self):
        self.write_fixture(probe_text=f"({self.names[0]} t)\n")
        with self.assertRaisesRegex(ValueError, '"missing_probes": 1459'):
            self.verify()
        self.write_fixture(probe_text="".join(f"({name} t)\n" for name in self.names)
                           + "(ghost t)\n")
        with self.assertRaisesRegex(ValueError, '"unknown_probes": 1'):
            self.verify()

    def test_malformed_probe_fails_closed(self):
        self.write_fixture(probe_text=f"({self.names[0]} t)\n({self.names[1]}\n")
        with self.assertRaisesRegex(ValueError, "probe reader failed"):
            self.verify()

    def test_legacy_scale_gap_reports_785_missing_owners(self):
        names = [f"name-{i:04d}" for i in range(1460)]
        self.write_fixture(names=names,
                           area_rows=[(name, "other") for name in names[:675]],
                           probe_text="".join(f"({name} t)\n" for name in names[:675]))
        with self.assertRaisesRegex(ValueError, '"missing_areas": 785'):
            self.verify()

    def test_font2_regrouping_preserves_all_twenty_forms(self):
        expected = ["font-put", "font-shape-gstring", "font-spec",
                    "font-variation-glyphs", "font-xlfd-name", "frame-font-cache",
                    "internal-char-font", "list-fonts", "open-font", "query-font"]
        census = expected + [f"filler-{i:04d}" for i in range(1450)]
        area_rows = [(name, "display") for name in expected]
        area_rows += [(name, "other") for name in census[10:]]
        broken = Path(__file__).with_name("fixtures") / "font-2-pre-repair.el"
        filler_probes = "".join(f"({name} t)\n" for name in census[10:])
        self.write_fixture(names=census, area_rows=area_rows,
                           probe_text=broken.read_text(encoding="utf-8") + filler_probes)
        with self.assertRaisesRegex(ValueError, '"unknown_probes"'):
            self.verify()

        repaired = Path(__file__).with_name("c-core-probes") / "font-2.el"
        repaired_text = repaired.read_text(encoding="utf-8")
        (self.probes / "font-2.el").write_text(repaired_text, encoding="utf-8")
        other_probe = self.probes / "fixture.el"
        other_probe.write_text(filler_probes, encoding="utf-8")
        font_probe_dir = self.root / "font-only"
        font_probe_dir.mkdir()
        (font_probe_dir / "font-2.el").write_text(repaired_text, encoding="utf-8")
        forms = VERIFY._probe_names(font_probe_dir, "emacs")
        self.assertEqual(len(forms), 20)
        self.assertEqual(set(forms), set(expected))
        self.assertEqual(set(Counter(forms).values()), {2})
        self.assertEqual(self.verify()["probe_forms"], 1470)


if __name__ == "__main__":
    unittest.main(verbosity=2)
