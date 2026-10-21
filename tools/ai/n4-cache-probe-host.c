#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
uint64_t nl_thread_registry[8];
uint64_t nl_root_pin_control[8];
static uint64_t env[32], state[32], root[4], entry[8];
static uint64_t marked[8], rewritten[8];
static unsigned marks, rewrites, slots;
uint64_t probe_root(uint64_t e) { return (uint64_t)root; }
uint64_t probe_entry(uint64_t e) { return (uint64_t)entry; }
uint64_t probe_mark(uint64_t v) { if(marks<8) marked[marks]=v; marks++; return 0; }
uint64_t probe_rewrite(uint64_t a) { if(rewrites<8) rewritten[rewrites]=a; rewrites++; return 0; }
uint64_t probe_slot(uint64_t a) { slots++; return 0; }
void *nl_alloc_bytes(uint64_t n, uint64_t align) { return calloc(1,n); }
uint64_t probe_unexpected_local(uint64_t env,uint64_t name) { fputs("unexpected local resolver in header fixture\n",stderr); exit(75); }
extern uint64_t nl_frame_stack_limit_setup(uint64_t,uint64_t);
extern uint64_t nl_frame_stack_limit(uint64_t,uint64_t);
extern uint64_t nl_n4_mark_env(uint64_t);
extern uint64_t nl_n4_rewrite_env(uint64_t);
int main(void) {
 unsigned fails=0,checks=0;
 #define CHECK(expr,label) do { checks++; if(!(expr)) { fails++; puts("FAIL " label); } } while(0)
 uint64_t raw16=123456,raw24=234568;
 /* Keep legacy accessor stubs and physical payload/slot fields consistent. */
 env[22]=(uint64_t)state; env[13]=9; entry[0]=(4<<8)|12; entry[1]=(uint64_t)entry; state[2]=raw16; state[3]=raw24;
 env[1]=(uint64_t)env; entry[2]=193;
 nl_frame_stack_limit_setup((uint64_t)env,(uint64_t)state);
 CHECK(state[2]==raw16,"raw lean-link16 preserved by setup");
 CHECK(state[3]==raw24,"raw watcher-link24 preserved by setup");
 CHECK(state[4]==(uint64_t)entry+6,"entry cache32");
 CHECK(state[5]==(uint64_t)env+6,"environment cache40");
 CHECK(state[6]==(uint64_t)root+6,"symbol cache48");
 CHECK(nl_frame_stack_limit((uint64_t)env,0)==48,"cached recursion limit read");
 CHECK(state[2]==raw16 && state[3]==raw24,"raw links preserved by limit");
 nl_n4_mark_env((uint64_t)env);
 CHECK(marks==3 && marked[0]==state[4] && marked[1]==state[5] && marked[2]==state[6],"GC marks exactly three cache WORDs32/40/48");
 nl_n4_rewrite_env((uint64_t)env);
 CHECK(rewrites==3 && rewritten[0]==(uint64_t)&state[4] && rewritten[1]==(uint64_t)&state[5] && rewritten[2]==(uint64_t)&state[6],"GC rewrites exactly three cache WORD edges32/40/48");
 CHECK(slots==0 && state[2]==raw16 && state[3]==raw24,"GC leaves raw links16/24 untouched");
 printf("N4-CACHE checks=%u failures=%u\n",checks,fails); return fails?1:0;
}
