#include "runtime.c"
#include "harness.h"
I cr_wf_bytecode_w_fast_entry(I a,I b) { exit(84); }
I cr_wf_bytecode_w_unbound_p(I a,I b) { exit(84); }
I cr_nl_word_payload(I a) { exit(84); }
I cr_wf_bytecode_call(I a,I b,I c,I d,I e) { exit(84); }
static I ck_main(I n, I r) {
  unsigned char code[4]={84,130,0,0}; I mem[2]={0}, bx[32]={0}; U s=0;
  for (I j=0;j<r;j++) for (I i=0;i<n;i++) {
    mem[0]=1;
    I state=cr_wf_bytecode_w_fast_run((I)(uintptr_t)code,4,(I)(uintptr_t)mem,2,0,0,0,1,0,(I)(uintptr_t)bx);
    s += state+mem[0];
  }
  return (I)s;
}
