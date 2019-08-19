
int init() {
   int nobjs=0, nwords=0;
   hp = (byte *) &heap; /* builtin heap */
   state = IFALSE;
   heap_metrics(&nwords, &nobjs);
   max_heap_mb = (W == 4) ? 4096 : 65535;
   nwords += nobjs + INITCELLS;
   memstart = genstart = fp = (word *) realloc(NULL, (nwords + MEMPAD)*W);
   if (!memstart) return 1;
   memend = memstart + nwords - MEMPAD;
   state = (word) load_heap(nobjs);
   return 0;
}

/* bvec → value library call test with preserved state */
word library_call(int arg) {
   word program_state = state;
   word res;
   state = IFALSE; 
   if (program_state == IFALSE) {
      printf("library_call: no program state - cannot continue");
      exit(1);
   }
   printf("libary call from state %d\n", state);
   res = vm((word *) program_state, onum(arg, 0));
   if(fixnump(res)) {
      printf("vm: Returned fixnum %d\n", immval(res));
   } else {
      printf("vm: Returned descriptor %d\n", res);
   }
   return res;
}

int main(int nargs, char **argv) {
   init();
   library_call(F(1));
   library_call(F(2));
   library_call(F(3));
   return 0;
}
