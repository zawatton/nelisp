Startup breakdown of the S8 ON-lane process, build28 (sha256 a10934c4...), row hash-get1024, N=16, warmups 0.
Instrumented copies of run/b28/benchmark-driver-native-on.el: drv-timed2.el (phase wrappers), drv-timed3.el (+ per-top-level-form marks).
Run: env -i HOME PATH BENCH_ROW=hash-get1024 BENCH_N=16 BENCH_HIGH=16 BENCH_WARMUPS=0 BENCH_ENGINE=nelisp NELISP_PRODUCER_IMAGE_SHA256=<image sha> nelisp-dynamic --load drv-timedN.el
Single runs, 2026-11-02; ms, one sample each (indicative, not a campaign).

bare image start, '(princ "ok")' file, median of 7:   19.4   (GNU 31.1 -Q --batch: 48.6)
same driver.el loaded directly (no preparation):      101.1
lane-new wrapper (python+bwrap+generated driver):     590.3
  python+bwrap+sha check overhead (590 - ~488):       ~100
generated driver, in-process (wall 455-488):
  image start before first form                        ~22
  forms 0-11 (stub defs, bench defuns)                 ~22
  read/define form 12 preamble                         ~22
  verify-pins x2 (elisp read + sha256 of 12 files)      36
  load producer.el from source                          26
  preflight                                             52
  install: fx-helper source load                        12
           require fxir-effects + fxir-analysis (src)   19
           recheck-inputs (second preflight)            34
           prepare-candidates                           10
           compile-candidates                           85
           install-candidates rest                       8
  form 16 registration/native-mapping checks            37
  form 18 benchmark main (input, check, validate)       50
GNU whole process for the same row at N=16384: 67.7 (campaign median).
