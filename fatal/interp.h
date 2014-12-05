struct exp;
typedef struct exp exp;
struct exp {
  enum {
    _const_exp,
    _var_exp,
    _if_exp,
    _mult_exp,
    _subr1_exp,
    _zero_exp,
    _letcc_exp,
    _throw_exp,
    _let_exp,
    _lambda_exp,
    _app_exp
  } tag;
  union {
    struct { void *_n; } _const;
    struct { void *_v; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_randr1; void *_randr2; } _mult;
    struct { void *_rand; } _subr1;
    struct { void *_rand; } _zero;
    struct { void *_body; } _letcc;
    struct { void *_vexp; void *_kexp; } _throw;
    struct { void *_vexp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *expr_const(void *n);
void *expr_var(void *v);
void *expr_if(void *test, void *conseq, void *alt);
void *expr_mult(void *randr1, void *randr2);
void *expr_subr1(void *rand);
void *expr_zero(void *rand);
void *expr_letcc(void *body);
void *expr_throw(void *vexp, void *kexp);
void *expr_let(void *vexp, void *body);
void *expr_lambda(void *body);
void *expr_app(void *rator, void *rand);

struct cont;
typedef struct cont cont;
struct cont {
  enum {
    _empty_cont,
    _if_cont,
    _multr__m__inner_cont,
    _multr__m__outer_cont,
    _subr1_cont,
    _zero_cont,
    _throwr__m__inner_cont,
    _throwr__m__outer_cont,
    _let_cont,
    _applyr__m__inner_cont,
    _applyr__m__outer_cont
  } tag;
  union {
    struct { void *_dismount; } _empty;
    struct { void *_conseq; void *_alt; void *_env; void *_k; } _if;
    struct { void *_rr1; void *_k; } _multr__m__inner;
    struct { void *_randr2; void *_env; void *_k; } _multr__m__outer;
    struct { void *_k; } _subr1;
    struct { void *_k; } _zero;
    struct { void *_a; } _throwr__m__inner;
    struct { void *_vexp; void *_env; } _throwr__m__outer;
    struct { void *_body; void *_env; void *_k; } _let;
    struct { void *_p; void *_k; } _applyr__m__inner;
    struct { void *_rand; void *_env; void *_k; } _applyr__m__outer;
  } u;
};

void *contr_empty(void *dismount);
void *contr_if(void *conseq, void *alt, void *env, void *k);
void *contr_multr__m__inner(void *rr1, void *k);
void *contr_multr__m__outer(void *randr2, void *env, void *k);
void *contr_subr1(void *k);
void *contr_zero(void *k);
void *contr_throwr__m__inner(void *a);
void *contr_throwr__m__outer(void *vexp, void *env);
void *contr_let(void *body, void *env, void *k);
void *contr_applyr__m__inner(void *p, void *k);
void *contr_applyr__m__outer(void *rand, void *env, void *k);

void *exprr__m__, *envr__m__, *kr__m__, *vr__m__, *numr__m__, *ar__m__, *cr__m__;

void (*pc)();

void valuer__m__of();
void applyr__m__k();
struct envr;
typedef struct envr envr;
struct envr {
  enum {
    _empty_envr,
    _extend_envr
  } tag;
  union {
    struct { char dummy; } _empty;
    struct { void *_arg; void *_env; } _extend;
  } u;
};

void *envrr_empty();
void *envrr_extend(void *arg, void *env);

void applyr__m__env();
struct clos;
typedef struct clos clos;
struct clos {
  enum {
    _closure_clos
  } tag;
  union {
    struct { void *_code; void *_env; } _closure;
  } u;
};

void *closr_closure(void *code, void *env);

void applyr__m__proc();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

