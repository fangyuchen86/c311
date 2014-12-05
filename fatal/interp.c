#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "interp.h"

void *expr_const(void *n) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_exp;
  _data->u._const._n = n;
  return (void *)_data;
}

void *expr_var(void *v) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_exp;
  _data->u._var._v = v;
  return (void *)_data;
}

void *expr_if(void *test, void *conseq, void *alt) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_exp;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *expr_mult(void *randr1, void *randr2) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_exp;
  _data->u._mult._randr1 = randr1;
  _data->u._mult._randr2 = randr2;
  return (void *)_data;
}

void *expr_subr1(void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_exp;
  _data->u._subr1._rand = rand;
  return (void *)_data;
}

void *expr_zero(void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_exp;
  _data->u._zero._rand = rand;
  return (void *)_data;
}

void *expr_letcc(void *body) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letcc_exp;
  _data->u._letcc._body = body;
  return (void *)_data;
}

void *expr_throw(void *vexp, void *kexp) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throw_exp;
  _data->u._throw._vexp = vexp;
  _data->u._throw._kexp = kexp;
  return (void *)_data;
}

void *expr_let(void *vexp, void *body) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_exp;
  _data->u._let._vexp = vexp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *expr_lambda(void *body) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_exp;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *expr_app(void *rator, void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_exp;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

void *contr_empty(void *dismount) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _empty_cont;
  _data->u._empty._dismount = dismount;
  return (void *)_data;
}

void *contr_if(void *conseq, void *alt, void *env, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_cont;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  _data->u._if._env = env;
  _data->u._if._k = k;
  return (void *)_data;
}

void *contr_multr__m__inner(void *rr1, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__inner_cont;
  _data->u._multr__m__inner._rr1 = rr1;
  _data->u._multr__m__inner._k = k;
  return (void *)_data;
}

void *contr_multr__m__outer(void *randr2, void *env, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__outer_cont;
  _data->u._multr__m__outer._randr2 = randr2;
  _data->u._multr__m__outer._env = env;
  _data->u._multr__m__outer._k = k;
  return (void *)_data;
}

void *contr_subr1(void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_cont;
  _data->u._subr1._k = k;
  return (void *)_data;
}

void *contr_zero(void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_cont;
  _data->u._zero._k = k;
  return (void *)_data;
}

void *contr_throwr__m__inner(void *a) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throwr__m__inner_cont;
  _data->u._throwr__m__inner._a = a;
  return (void *)_data;
}

void *contr_throwr__m__outer(void *vexp, void *env) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throwr__m__outer_cont;
  _data->u._throwr__m__outer._vexp = vexp;
  _data->u._throwr__m__outer._env = env;
  return (void *)_data;
}

void *contr_let(void *body, void *env, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_cont;
  _data->u._let._body = body;
  _data->u._let._env = env;
  _data->u._let._k = k;
  return (void *)_data;
}

void *contr_applyr__m__inner(void *p, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _applyr__m__inner_cont;
  _data->u._applyr__m__inner._p = p;
  _data->u._applyr__m__inner._k = k;
  return (void *)_data;
}

void *contr_applyr__m__outer(void *rand, void *env, void *k) {
cont* _data = (cont*)malloc(sizeof(cont));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _applyr__m__outer_cont;
  _data->u._applyr__m__outer._rand = rand;
  _data->u._applyr__m__outer._env = env;
  _data->u._applyr__m__outer._k = k;
  return (void *)_data;
}

void *envrr_empty() {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _empty_envr;
  return (void *)_data;
}

void *envrr_extend(void *arg, void *env) {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extend_envr;
  _data->u._extend._arg = arg;
  _data->u._extend._env = env;
  return (void *)_data;
}

void *closr_closure(void *code, void *env) {
clos* _data = (clos*)malloc(sizeof(clos));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _closure_clos;
  _data->u._closure._code = code;
  _data->u._closure._env = env;
  return (void *)_data;
}

void valuer__m__of()
{
exp* _c = (exp*)exprr__m__;
switch (_c->tag) {
case _const_exp: {
void *n = _c->u._const._n;
vr__m__ = (void *)n;
pc = &applyr__m__k;
break; }
case _var_exp: {
void *v = _c->u._var._v;
numr__m__ = (void *)v;
pc = &applyr__m__env;
break; }
case _if_exp: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
kr__m__ = (void *)contr_if(conseq,alt,envr__m__,kr__m__);
exprr__m__ = (void *)test;
pc = &valuer__m__of;
break; }
case _mult_exp: {
void *randr1 = _c->u._mult._randr1;
void *randr2 = _c->u._mult._randr2;
kr__m__ = (void *)contr_multr__m__outer(randr2,envr__m__,kr__m__);
exprr__m__ = (void *)randr1;
pc = &valuer__m__of;
break; }
case _subr1_exp: {
void *rand = _c->u._subr1._rand;
kr__m__ = (void *)contr_subr1(kr__m__);
exprr__m__ = (void *)rand;
pc = &valuer__m__of;
break; }
case _zero_exp: {
void *rand = _c->u._zero._rand;
kr__m__ = (void *)contr_zero(kr__m__);
exprr__m__ = (void *)rand;
pc = &valuer__m__of;
break; }
case _letcc_exp: {
void *body = _c->u._letcc._body;
exprr__m__ = (void *)body;
envr__m__ = (void *)envrr_extend(kr__m__,envr__m__);
pc = &valuer__m__of;
break; }
case _throw_exp: {
void *vexp = _c->u._throw._vexp;
void *kexp = _c->u._throw._kexp;
kr__m__ = (void *)contr_throwr__m__outer(vexp,envr__m__);
exprr__m__ = (void *)kexp;
pc = &valuer__m__of;
break; }
case _let_exp: {
void *vexp = _c->u._let._vexp;
void *body = _c->u._let._body;
kr__m__ = (void *)contr_let(body,envr__m__,kr__m__);
exprr__m__ = (void *)vexp;
pc = &valuer__m__of;
break; }
case _lambda_exp: {
void *body = _c->u._lambda._body;
vr__m__ = (void *)closr_closure(body,envr__m__);
pc = &applyr__m__k;
break; }
case _app_exp: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
kr__m__ = (void *)contr_applyr__m__outer(rand,envr__m__,kr__m__);
exprr__m__ = (void *)rator;
pc = &valuer__m__of;
break; }
}
}

void applyr__m__k()
{
cont* _c = (cont*)kr__m__;
switch (_c->tag) {
case _empty_cont: {
void *dismount = _c->u._empty._dismount;
_trstr *trstr = (_trstr *)dismount;
longjmp(*trstr->jmpbuf, 1);
break; }
case _if_cont: {
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
void *env = _c->u._if._env;
void *k = _c->u._if._k;
if(vr__m__) {
  kr__m__ = (void *)k;
exprr__m__ = (void *)conseq;
envr__m__ = (void *)env;
pc = &valuer__m__of;

} else {
  kr__m__ = (void *)k;
exprr__m__ = (void *)alt;
envr__m__ = (void *)env;
pc = &valuer__m__of;

}
break; }
case _multr__m__inner_cont: {
void *rr1 = _c->u._multr__m__inner._rr1;
void *k = _c->u._multr__m__inner._k;
kr__m__ = (void *)k;
vr__m__ = (void *)(void *)((int)rr1 * (int)vr__m__);
pc = &applyr__m__k;
break; }
case _multr__m__outer_cont: {
void *randr2 = _c->u._multr__m__outer._randr2;
void *env = _c->u._multr__m__outer._env;
void *k = _c->u._multr__m__outer._k;
kr__m__ = (void *)contr_multr__m__inner(vr__m__,k);
exprr__m__ = (void *)randr2;
envr__m__ = (void *)env;
pc = &valuer__m__of;
break; }
case _subr1_cont: {
void *k = _c->u._subr1._k;
kr__m__ = (void *)k;
vr__m__ = (void *)(void *)((int)vr__m__ - (int)(void *)1);
pc = &applyr__m__k;
break; }
case _zero_cont: {
void *k = _c->u._zero._k;
kr__m__ = (void *)k;
vr__m__ = (void *)(vr__m__ == 0);
pc = &applyr__m__k;
break; }
case _throwr__m__inner_cont: {
void *a = _c->u._throwr__m__inner._a;
kr__m__ = (void *)a;
pc = &applyr__m__k;
break; }
case _throwr__m__outer_cont: {
void *vexp = _c->u._throwr__m__outer._vexp;
void *env = _c->u._throwr__m__outer._env;
kr__m__ = (void *)contr_throwr__m__inner(vr__m__);
exprr__m__ = (void *)vexp;
envr__m__ = (void *)env;
pc = &valuer__m__of;
break; }
case _let_cont: {
void *body = _c->u._let._body;
void *env = _c->u._let._env;
void *k = _c->u._let._k;
kr__m__ = (void *)k;
exprr__m__ = (void *)body;
envr__m__ = (void *)envrr_extend(vr__m__,env);
pc = &valuer__m__of;
break; }
case _applyr__m__inner_cont: {
void *p = _c->u._applyr__m__inner._p;
void *k = _c->u._applyr__m__inner._k;
cr__m__ = (void *)p;
ar__m__ = (void *)vr__m__;
kr__m__ = (void *)k;
pc = &applyr__m__proc;
break; }
case _applyr__m__outer_cont: {
void *rand = _c->u._applyr__m__outer._rand;
void *env = _c->u._applyr__m__outer._env;
void *k = _c->u._applyr__m__outer._k;
kr__m__ = (void *)contr_applyr__m__inner(vr__m__,k);
exprr__m__ = (void *)rand;
envr__m__ = (void *)env;
pc = &valuer__m__of;
break; }
}
}

void applyr__m__env()
{
envr* _c = (envr*)envr__m__;
switch (_c->tag) {
case _empty_envr: {
printf("unbound variable");break; }
case _extend_envr: {
void *arg = _c->u._extend._arg;
void *env = _c->u._extend._env;
if((numr__m__ == 0)) {
  vr__m__ = (void *)arg;
pc = &applyr__m__k;

} else {
  envr__m__ = (void *)env;
numr__m__ = (void *)(void *)((int)numr__m__ - 1);
pc = &applyr__m__env;

}
break; }
}
}

void applyr__m__proc()
{
clos* _c = (clos*)cr__m__;
switch (_c->tag) {
case _closure_clos: {
void *code = _c->u._closure._code;
void *env = _c->u._closure._env;
exprr__m__ = (void *)code;
envr__m__ = (void *)envrr_extend(ar__m__,env);
pc = &valuer__m__of;
break; }
}
}

int main()
{
exprr__m__ = (void *)expr_app(expr_lambda(expr_app(expr_app(expr_var((void *)0),expr_var((void *)0)),expr_const((void *)5))),expr_lambda(expr_lambda(expr_if(expr_zero(expr_var((void *)0)),expr_const((void *)1),expr_mult(expr_var((void *)0),expr_app(expr_app(expr_var((void *)1),expr_var((void *)1)),expr_subr1(expr_var((void *)0))))))));
envr__m__ = (void *)envrr_empty();
pc = &valuer__m__of;
mount_tram();
printf("Factorial of 5: %d\n", (int)vr__m__);exprr__m__ = (void *)expr_mult(expr_const((void *)2),expr_letcc(expr_mult(expr_const((void *)5),expr_throw(expr_mult(expr_const((void *)2),expr_const((void *)6)),expr_var((void *)0)))));
envr__m__ = (void *)envrr_empty();
pc = &valuer__m__of;
mount_tram();
printf("letcc/throw, 24?: %d\n", (int)vr__m__);exprr__m__ = (void *)expr_let(expr_lambda(expr_lambda(expr_if(expr_zero(expr_var((void *)0)),expr_const((void *)1),expr_mult(expr_var((void *)0),expr_app(expr_app(expr_var((void *)1),expr_var((void *)1)),expr_subr1(expr_var((void *)0))))))),expr_app(expr_app(expr_var((void *)0),expr_var((void *)0)),expr_const((void *)5)));
envr__m__ = (void *)envrr_empty();
pc = &valuer__m__of;
mount_tram();
printf("comes out to 120?: %d\n", (int)vr__m__);}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
kr__m__= (void *)contr_empty(dismount);
for(;;) {
pc();
}
}
return 0;
}
