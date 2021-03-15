#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<stdarg.h>
#include<memory.h>
#include<z3.h>

/** 
   \defgroup theory_plugin_ex Theory plugin examples
*/
/*@{*/
/**
   \brief exit gracefully in case of error.
*/
void exitf(const char* message) 
{
  fprintf(stderr,"BUG: %s.\n", message);
  exit(1);
}

/**
   \brief Simpler error handler.
 */
void error_handler(Z3_error_code e) 
{
    printf("Error code: %d\n", e);
    exitf("incorrect use of Z3");
}

/**
   \brief Create a logical context.  

   Enable model construction. Other configuration parameters can be passed in the cfg variable.

   Also enable tracing to stderr and register custom error handler.
*/
Z3_context mk_context_custom(Z3_config cfg, Z3_error_handler err) 
{
    Z3_context ctx;
    
    Z3_set_param_value(cfg, "MODEL", "true");
    ctx = Z3_mk_context(cfg);
#ifdef TRACING
    Z3_trace_to_stderr(ctx);
#endif
    Z3_set_error_handler(ctx, err);
    
    return ctx;
}

/**
   \brief Create a logical context.

   Enable model construction only.

   Also enable tracing to stderr and register standard error handler.
*/
Z3_context mk_context() 
{
    Z3_config  cfg;
    Z3_context ctx;
    cfg = Z3_mk_config();
    ctx = mk_context_custom(cfg, error_handler);
    Z3_del_config(cfg);
    return ctx;
}


/**
   \brief Create a variable using the given name and type.
*/
Z3_ast mk_var(Z3_context ctx, const char * name, Z3_sort ty) 
{
    Z3_symbol   s  = Z3_mk_string_symbol(ctx, name);
    return Z3_mk_const(ctx, s, ty);
}

/**
   \brief Create a boolean variable using the given name.
*/
Z3_ast mk_bool_var(Z3_context ctx, const char * name) 
{
    Z3_sort ty = Z3_mk_bool_sort(ctx);
    return mk_var(ctx, name, ty);
}

/**
   \brief Create an integer variable using the given name.
*/
Z3_ast mk_int_var(Z3_context ctx, const char * name) 
{
    Z3_sort ty = Z3_mk_int_sort(ctx);
    return mk_var(ctx, name, ty);
}

/**
   \brief Create a Z3 integer node using a C int. 
*/
Z3_ast mk_int(Z3_context ctx, int v) 
{
    Z3_sort ty = Z3_mk_int_sort(ctx);
    return Z3_mk_int(ctx, v, ty);
}


/**
   \brief Create the unary function application: <tt>(f x)</tt>.
*/
Z3_ast mk_unary_app(Z3_context ctx, Z3_func_decl f, Z3_ast x) 
{
    Z3_ast args[1] = {x};
    return Z3_mk_app(ctx, f, 1, args);
}

/**
   \brief Create the binary function application: <tt>(f x y)</tt>.
*/
Z3_ast mk_binary_app(Z3_context ctx, Z3_func_decl f, Z3_ast x, Z3_ast y) 
{
    Z3_ast args[2] = {x, y};
    return Z3_mk_app(ctx, f, 2, args);
}

/**
   \brief Check whether the logical context is satisfiable, and compare the result with the expected result.
   If the context is satisfiable, then display the model.
*/
void check(Z3_context ctx, Z3_lbool expected_result)
{
    Z3_model m      = 0;
    Z3_lbool result = Z3_check_and_get_model(ctx, &m);
    switch (result) {
    case Z3_L_FALSE:
        printf("unsat\n");
        break;
    case Z3_L_UNDEF:
        printf("unknown\n");
        printf("potential model:\n%s\n", Z3_model_to_string(ctx, m));
        break;
    case Z3_L_TRUE:
        printf("sat\n%s\n", Z3_model_to_string(ctx, m));
        break;
    }
    if (m) {
        Z3_del_model(ctx, m);
    }
    if (result != expected_result) {
        exitf("unexpected result");
    }
}

/**
   \brief Display equivalence class containing \c n.
*/
void display_eqc(Z3_theory t, Z3_ast n) {
    Z3_context c = Z3_theory_get_context(t);
    Z3_ast curr = n;
    printf("  ----- begin eqc of %s", Z3_ast_to_string(c, n));
    printf(", root: %s\n", Z3_ast_to_string(c, Z3_theory_get_eqc_root(t, n)));
    do {
        printf("  %s\n", Z3_ast_to_string(c, curr));
        curr = Z3_theory_get_eqc_next(t, curr);
    }
    while (curr != n);
    printf("  ----- end of eqc\n");
}

/**
   \brief Display the parent theory operators of the equivalence class containing \c n.
*/
void display_eqc_parents(Z3_theory t, Z3_ast n) {
    Z3_context c = Z3_theory_get_context(t);
    Z3_ast curr = n;
    printf("  ----- begin eqc (theory) parents of %s\n", Z3_ast_to_string(c, n));
    do {
        unsigned num_parents = Z3_theory_get_num_parents(t, curr);
        unsigned i;
        for (i = 0; i < num_parents; i++) {
            Z3_ast p = Z3_theory_get_parent(t, curr, i);
            printf("  %s\n", Z3_ast_to_string(c, p));
        }
        curr = Z3_theory_get_eqc_next(t, curr);
    }
    while (curr != n);
    printf("  ----- end of eqc (theory) parents\n");
}

void display_new_eq(Z3_theory t, Z3_ast n1, Z3_ast n2) {
    printf("====== begin new equality\n");
    display_eqc(t, n1);
    display_eqc_parents(t, n1);
    printf("  ==\n");
    display_eqc(t, n2);
    display_eqc_parents(t, n2);
    printf("====== end new equality\n");
}

/**
   \brief If the equivalence class containing \c n contains a theory value, then return it.
   Otherwise, return 0.
   
   \remark An equivalence class may contain at most one theory value
   since theory values are different by definition.
*/
Z3_ast get_eqc_value(Z3_theory t, Z3_ast n) {
    Z3_ast curr = n;
    do {
        if (Z3_theory_is_value(t, curr))
            return curr;
        curr = Z3_theory_get_eqc_next(t, curr);
    }
    while (curr != n);
    return 0;
}

/** 
    @name Simple Theory
    The theory has a binary function f, and a unit element u.
    The theory axioms are:
    - <tt>forall X. f(X, u) = X</tt>
    - <tt>forall X. f(u, X) = X</tt>
    - <tt>forall X. p(X, X)</tt>
*/
/*@{*/

/**
   \brief Data-structure for storing theory specific data.
*/
struct _SimpleTheoryData {
    Z3_sort      S; /*!< Interpreted theory sort */
    Z3_func_decl f; /*!< Interpreted theory binary function symbol */ 
    Z3_func_decl p; /*!< Interpreted theory predicate */
    Z3_ast       u; /*!< unit for f */
};

/**
   \brief Data-structure for storing theory specific data.
*/
typedef struct _SimpleTheoryData SimpleTheoryData;

/**
   \brief Callback: invoked when \c t is deleted.
   This callback can be used to free theory specific data-structures.
*/
void Th_delete(Z3_theory t) {
    SimpleTheoryData * td = (SimpleTheoryData *)Z3_theory_get_ext_data(t);
    printf("Delete\n");
    free(td);
}

/**
   \brief The reduce_app callback can be used to extend Z3's simplifier.
   The simplifier is used to preprocess formulas.
*/
Z3_bool Th_reduce_app(Z3_theory t, Z3_func_decl d, unsigned n, Z3_ast const args[], Z3_ast * result) {
    SimpleTheoryData * td = (SimpleTheoryData*)Z3_theory_get_ext_data(t);
    if (d == td->f) {
        if (args[0] == td->u) {
            *result = args[1];
            return Z3_TRUE;
        }
        else if (args[1] == td->u) {
            *result = args[0];
            return Z3_TRUE;
        }
    }
    else if (d == td->p) {
        if (args[0] == args[1]) {
            *result = Z3_mk_true(Z3_theory_get_context(t));
            return Z3_TRUE;
        }
    }
    return Z3_FALSE; // failed to simplify
}

/**
   \brief Callback: invoked when \c n is finally added to the logical context. 
   \c n is an application of the form <tt>g(...)</tt> where \c g is an
   interpreted function symbol of \c t.
*/
void Th_new_app(Z3_theory t, Z3_ast n) {
    Z3_context c = Z3_theory_get_context(t);
    printf("New app: %s\n", Z3_ast_to_string(c, n));
}

/**
   \brief Callback: invoked when \c n is finally added to the logical context. 
   \c n is an expression of sort \c s, where \c s is an interpreted sort of \c t.
*/
void Th_new_elem(Z3_theory t, Z3_ast n) {
    Z3_context c = Z3_theory_get_context(t);
    printf("New elem: %s\n", Z3_ast_to_string(c, n));
}

/**
   \brief Callback: invoked when Z3 starts the search for a satisfying assignment.
*/
void Th_init_search(Z3_theory t) {
    printf("Starting search\n");
}

/**
   \brief Callback: invoked when Z3 creates a case-split (aka backtracking point).
*/
void Th_push(Z3_theory t) {
    printf("Push\n");
}

/**
   \brief Callback: invoked when Z3 backtracks a case-split.

   \see Th_push
*/
void Th_pop(Z3_theory t) {
    printf("Pop\n");
}

/**
   \brief Callback: invoked when the logical context containing \c t is reset.
*/
void Th_reset(Z3_theory t) {
    printf("Reset\n");
}

/**
   \brief Callback: invoked when Z3 restarts the search for a satisfying assignment.
*/
void Th_restart(Z3_theory t) {
    printf("Restart\n");
}

/**
   \brief Instantiate the unit axiom for the parents of \c n because
   \c n is now equal to unit u in the logical context.
*/
void apply_unit_axiom_for_parents_of(Z3_theory t, Z3_ast n) {
    SimpleTheoryData * td = (SimpleTheoryData*)Z3_theory_get_ext_data(t);
    Z3_context c = Z3_theory_get_context(t);
    /*
      The following idiom is the standard approach for traversing
      applications of the form
      
      g(..., n', ...) 
      where
      - g is an interpreted function symbol of \c t.
      - n' is in the same equivalence class of n.
    */
    Z3_ast n_prime = n;
    do {
        unsigned num_parents = Z3_theory_get_num_parents(t, n_prime);
        unsigned i;
        for (i = 0; i < num_parents; i++) {
            Z3_app parent = Z3_to_app(c, Z3_theory_get_parent(t, n_prime, i));
            /* check whether current parent is of the form f(a, n_prime) */
            if (Z3_get_app_decl(c, parent) == td->f && Z3_get_app_arg(c, parent, 1) == n_prime) {
                /* assert f(a, u) = a */
                Z3_ast a = Z3_get_app_arg(c, parent, 0);
                Z3_theory_assert_axiom(t, Z3_mk_eq(c, mk_binary_app(c, td->f, a, td->u), a));
                /* Instead of asserting f(a, u) = a, we could also have asserted
                   the clause:
                   (not (n_prime = u)) or (f(a, n_prime) = a)
                   
                   However, this solution is wasteful, because the axiom 
                   assert f(a, u) = a is simpler and more general.
                   Note that n_prime is in the equivalence class of n,
                   and n is now equal to u. So, n_prime is also equal to u.
                   Then, using congruence, Z3 will also deduce that f(a, n_prime) = a
                   using the simpler axiom.
                */
            }
            /* check whether current parent is of the form f(n_prime, a) */
            if (Z3_get_app_decl(c, parent) == td->f && Z3_get_app_arg(c, parent, 0) == n_prime) {
                /* assert f(u, a) = a */
                Z3_ast a = Z3_get_app_arg(c, parent, 1);
                Z3_theory_assert_axiom(t, Z3_mk_eq(c, mk_binary_app(c, td->f, td->u, a), a));
            }
        }
        n_prime = Z3_theory_get_eqc_next(t, n_prime);
    }
    while (n_prime != n);
}

/**
   \brief Callback: invoked when <tt>n1 = n2</tt> is true in the logical context.
   
   Z3 will only invoke this callback using expressions \c n1 and \c n2 s.t.
   #Th_new_app or #Th_new_elem was invoked for them.
*/
void Th_new_eq(Z3_theory t, Z3_ast n1, Z3_ast n2) {
    SimpleTheoryData * td = (SimpleTheoryData*)Z3_theory_get_ext_data(t);
    Z3_context c = Z3_theory_get_context(t);
    display_new_eq(t, n1, n2);
    if (Z3_theory_get_eqc_root(t, n1) == Z3_theory_get_eqc_root(t, td->u)) {
        apply_unit_axiom_for_parents_of(t, n2);
    }
    if (Z3_theory_get_eqc_root(t, n2) == Z3_theory_get_eqc_root(t, td->u)) {
        apply_unit_axiom_for_parents_of(t, n1);
    }
}

/**
   \brief Callback: invoked when <tt>n1 = n2</tt> is false in the logical context.
   
   Z3 will only invoke this callback using expressions \c n1 and \c n2 s.t.
   #Th_new_app or #Th_new_elem was invoked for them.
*/
void Th_new_diseq(Z3_theory t, Z3_ast n1, Z3_ast n2) {
    Z3_context c = Z3_theory_get_context(t);
    printf("New disequality: %s ", Z3_ast_to_string(c, n1));
    printf("!= %s\n", Z3_ast_to_string(c, n2));
}

/**
   \brief Callback: invoked when \c n becomes relevant in the current search branch.
   Irrelevant expressions may be ignored by the theory solver.

   Z3 will only invoke this callback using a expression \c n s.t.
   #Th_new_app or #Th_new_elem was invoked for it.
*/
void Th_new_relevant(Z3_theory t, Z3_ast n) {
    Z3_context c = Z3_theory_get_context(t);
    printf("Relevant: %s\n", Z3_ast_to_string(c, n));
}

/**
   \brief Callback: invoked when \c n is assigned to true/false.

   Z3 will only invoke this callback using a expression \c n s.t.
   #Th_new_app or #Th_new_elem was invoked for it.
*/
void Th_new_assignment(Z3_theory t, Z3_ast n, Z3_bool v) {
    Z3_context c = Z3_theory_get_context(t);
    printf("Assigned: %s --> %d\n", Z3_ast_to_string(c, n), v);
}

/**
   \brief Callback: invoked before Z3 starts building a model.
   This callback can be used to perform expensive operations lazily.
*/
Z3_bool Th_final_check(Z3_theory t) {
    printf("Final check\n");
    return Z3_TRUE;
}

/**
   \brief Simple theory example.
   The theory has a binary function f, and a unit element u.
   
   The theory axioms are:
   - <tt>forall X. f(X, u) = X</tt>
   - <tt>forall X. f(u, X) = X</tt>
*/
Z3_theory mk_simple_theory(Z3_context ctx) {
    Z3_sort f_domain[2];
    Z3_symbol s_name      = Z3_mk_string_symbol(ctx, "S");
    Z3_symbol f_name      = Z3_mk_string_symbol(ctx, "f");
    Z3_symbol p_name      = Z3_mk_string_symbol(ctx, "p");
    Z3_symbol u_name      = Z3_mk_string_symbol(ctx, "u");
    Z3_sort B             = Z3_mk_bool_sort(ctx);
    SimpleTheoryData * td = (SimpleTheoryData*)malloc(sizeof(SimpleTheoryData));  
    Z3_theory Th          = Z3_mk_theory(ctx, "simple_th", td);
    td->S                 = Z3_theory_mk_sort(ctx, Th, s_name); 
    f_domain[0] = td->S; f_domain[1] = td->S;
    td->f                 = Z3_theory_mk_func_decl(ctx, Th, f_name, 2, f_domain, td->S);
    td->p                 = Z3_theory_mk_func_decl(ctx, Th, p_name, 1, &td->S, B); 
    td->u                 = Z3_theory_mk_constant(ctx, Th, u_name, td->S);

    Z3_set_delete_callback(Th, Th_delete);
    Z3_set_reduce_app_callback(Th, Th_reduce_app);
    Z3_set_new_app_callback(Th, Th_new_app);
    Z3_set_new_elem_callback(Th, Th_new_elem);
    Z3_set_init_search_callback(Th, Th_init_search);
    Z3_set_push_callback(Th, Th_push);
    Z3_set_pop_callback(Th, Th_pop);
    Z3_set_reset_callback(Th, Th_reset);
    Z3_set_restart_callback(Th, Th_restart);
    Z3_set_new_eq_callback(Th, Th_new_eq);
    Z3_set_new_diseq_callback(Th, Th_new_diseq);
    Z3_set_new_relevant_callback(Th, Th_new_relevant);
    Z3_set_new_assignment_callback(Th, Th_new_assignment);
    Z3_set_final_check_callback(Th, Th_final_check);
    return Th;
}

/**
   \brief Test simple theory simplifier.
*/
void simple_example1() 
{
    Z3_ast a, b, c, f1, f2, f3, r, i;
    Z3_context ctx;
    Z3_theory Th;
    SimpleTheoryData * td;
    printf("\nsimple_example1\n");
    ctx = mk_context();
    Th = mk_simple_theory(ctx);
    td = (SimpleTheoryData*)Z3_theory_get_ext_data(Th);
    a  = mk_var(ctx, "a", td->S);
    b  = mk_var(ctx, "b", td->S);
    c  = mk_var(ctx, "c", td->S);
    i  = Z3_mk_ite(ctx, Z3_mk_eq(ctx, td->u, td->u), c, td->u);
    f1 = mk_binary_app(ctx, td->f, a, i);
    f2 = mk_binary_app(ctx, td->f, td->u, f1);
    f3 = mk_binary_app(ctx, td->f, b, f2);
    printf("%s\n==>\n", Z3_ast_to_string(ctx, f3));
    r  = Z3_simplify(ctx, f3);
    printf("%s\n",      Z3_ast_to_string(ctx, r));

    Z3_del_context(ctx);
}

/**
   \brief Test simple theory.
*/
void simple_example2() 
{
    Z3_ast a, b, c, d, f1;
    Z3_ast args[2];
    Z3_context ctx;
    Z3_theory Th;
    SimpleTheoryData * td;
    printf("\nsimple_example2\n");
    ctx = mk_context();
    Th = mk_simple_theory(ctx);
    td = (SimpleTheoryData*)Z3_theory_get_ext_data(Th);

    a  = mk_var(ctx, "a", td->S);
    b  = mk_var(ctx, "b", td->S);
    c  = mk_var(ctx, "c", td->S);
    d  = mk_var(ctx, "d", td->S);
    f1 = mk_binary_app(ctx, td->f, a, b);
    /* asserting a = c \/ b = d */
    args[0] = Z3_mk_eq(ctx, a, c);
    args[1] = Z3_mk_eq(ctx, b, d);
    Z3_assert_cnstr(ctx, Z3_mk_or(ctx, 2, args));
    /* asserting c = u */
    Z3_assert_cnstr(ctx, Z3_mk_eq(ctx, c, td->u));
    /* asserting d = u */
    Z3_assert_cnstr(ctx, Z3_mk_eq(ctx, d, td->u));
    /* asserting b != f(a,b) */
    Z3_assert_cnstr(ctx, Z3_mk_not(ctx, Z3_mk_eq(ctx, b, f1)));
    /* asserting a != f(a,b) */
    Z3_assert_cnstr(ctx, Z3_mk_not(ctx, Z3_mk_eq(ctx, a, f1)));
    /* asserting p(a) */
    Z3_assert_cnstr(ctx, mk_unary_app(ctx, td->p, a));
    /* asserting !p(b) */
    Z3_assert_cnstr(ctx, Z3_mk_not(ctx, mk_unary_app(ctx, td->p, b)));

    // printf("Context:\n%s\n", Z3_context_to_string(ctx));
    check(ctx, Z3_L_FALSE);
    Z3_del_context(ctx);
}

/*@}*/

/** 
  @name Procedural Attachment Example
 
  The API for creating external theories can be used to implement
  procedural attachments in Z3. The idea is to use C functions to
  give the interpretation of function and predicate symbols. 
  
  The general template consists in:

  - Defining theory values for each C value that we want to 
  manipulate in a Z3 formula. These values may be created on
  demand.

  - Defining a theory function declaration for each C function
  we want to make available in a Z3 formula.
  
  In this file, we show how to create a procedural attachment
  for the C function \c strcmp.
  Our theory provides a function for creating string values,
  and the implementation consists in the following simple rule:
  
  <pre>
  t1 = str_value1
  t2 = str_value2
  strcmp(str_of(str_value1), str_of(str_value2)) == 0
  ===>
  Compare(t1, t2) = true
  </pre>

  This rule should be read as 
  <pre>
  IF
    - the term \c t1 is known to be equal to a theory value str_value1, and
    - the term \c t2 is known to be equal to a theory value str_value2, and
    - the C function strcmp returns 0 when the strings associated with str_value1 and str_value2 are used as arguments
  THEN
    - The theory atom <tt>Compare(t1, t2)</tt> must be true.
  </pre>

  Similarly, we have

  <tt>
  c1 = str_value1
  c2 = str_value2
  strcmp(str_of(str_value1), str_of(str_value2)) != 0
  ===>
  Compare(v1, v2) = false
  </tt>

  This solution has its limitations. For example, the theory does not restrict
  the interpretation of <tt>Compare(t1, t2)</tt> when \c t1 and \c t2 are
  not known to be equal to any theory value. Our example solver will simply
  return \c unknown in this case.
*/
/*@{*/

/**
   \brief Theory specific data-structures.
*/
struct _PATheoryData {
    Z3_sort      String;
    Z3_func_decl Compare;
};

/**
   \brief Theory specific data-structures.
*/
typedef struct _PATheoryData PATheoryData;

/**
   \brief Create a new string interpreted value using the C string \c str.
*/
Z3_ast PATh_mk_string_value(Z3_theory t, char const * str) {
    Z3_context ctx      = Z3_theory_get_context(t);
    PATheoryData * td   = (PATheoryData *)Z3_theory_get_ext_data(t);
    /* store the string as the name of the new interpreted value */
    Z3_symbol str_sym   = Z3_mk_string_symbol(ctx, str);
    return Z3_theory_mk_value(ctx, t, str_sym, td->String);
}

/**
   \brief Callback: delete for PATheory
*/
void PATh_delete(Z3_theory t) {
    PATheoryData * td = (PATheoryData *)Z3_theory_get_ext_data(t);
    printf("Delete\n");
    free(td);
}

/**
   \brief Compare the theory values in the equivalence classes of \c n1 and \c n2 using strcmp.
   
   Return Z3_L_UNDEF if the equivalence class of \c n1 or \c n2 does not contain a theory value.
*/
Z3_lbool Compare(Z3_theory t, Z3_ast n1, Z3_ast n2) {
    Z3_context ctx      = Z3_theory_get_context(t);
    printf("Compare(%s", Z3_ast_to_string(ctx, n1));
    printf(", %s)", Z3_ast_to_string(ctx, n2));
    if (Z3_theory_is_value(t, n1) && Z3_theory_is_value(t, n2)) {
        Z3_func_decl d1     = Z3_get_app_decl(ctx, Z3_to_app(ctx, n1));
        Z3_func_decl d2     = Z3_get_app_decl(ctx, Z3_to_app(ctx, n2));
        Z3_symbol    s1     = Z3_get_decl_name(ctx, d1);
        Z3_symbol    s2     = Z3_get_decl_name(ctx, d2);
        Z3_string    str1   = Z3_get_symbol_string(ctx, s1);
        Z3_string    str2;
        int strcmp_result;
        /* the next call to Z3_get_symbol_string will invalidate str1, so we need to create a copy */
        char * str1_copy    = strdup(str1);
        str2                = Z3_get_symbol_string(ctx, s2);
        strcmp_result       = strcmp(str1_copy, str2);
        free(str1_copy);
        if (strcmp_result == 0) {
            printf(" = true\n");
            return Z3_L_TRUE;
        }
        else {
            printf(" = false\n");
            return Z3_L_FALSE;
        }
    }
    printf(" = unknown\n");
    return Z3_L_UNDEF;
}

/**
   \brief Callback: reduce_app for PATheory
*/
Z3_bool PATh_reduce_app(Z3_theory t, Z3_func_decl d, unsigned n, Z3_ast const args[], Z3_ast * result) {
    Z3_context ctx    = Z3_theory_get_context(t);
    PATheoryData * td = (PATheoryData*)Z3_theory_get_ext_data(t);
    if (d == td->Compare) {
        switch (Compare(t, args[0], args[1])) {
        case Z3_L_TRUE:
            *result = Z3_mk_true(ctx);
            return Z3_TRUE; 
        case Z3_L_FALSE:
            *result = Z3_mk_false(ctx);
            return Z3_TRUE;
        case Z3_L_UNDEF:
            return Z3_FALSE; // failed to simplify
        }
    }
    return Z3_FALSE; // failed to simplify
}

/**
   \brief Try to apply Compare axiom using the fact that <tt>n = v</tt>, where \c v is a theory value.
*/
void apply_compare_axiom_for_parents_of(Z3_theory t, Z3_ast n, Z3_ast v) {
    PATheoryData * td = (PATheoryData*)Z3_theory_get_ext_data(t);
    Z3_context c = Z3_theory_get_context(t);
    Z3_ast n_prime = Z3_theory_get_eqc_root(t, n);
    do {
        unsigned num_parents = Z3_theory_get_num_parents(t, n_prime);
        unsigned i;
        for (i = 0; i < num_parents; i++) {
            Z3_app parent = Z3_to_app(c, Z3_theory_get_parent(t, n_prime, i));
            if (Z3_get_app_decl(c, parent) == td->Compare) {
                Z3_ast arg1 = Z3_get_app_arg(c, parent, 0);
                Z3_ast arg2 = Z3_get_app_arg(c, parent, 1);
                if (Z3_theory_get_eqc_root(t, arg1) == n)
                    arg1 = v;
                else
                    arg1 = get_eqc_value(t, arg1);
                if (Z3_theory_get_eqc_root(t, arg2) == n)
                    arg2 = v;
                else
                    arg2 = get_eqc_value(t, arg2);
                if (arg1 != 0 && arg2 != 0) {
                    switch (Compare(t, arg1, arg2)) {
                    case Z3_L_TRUE:
                        // assert axiom: Compare(arg1, arg2)
                        Z3_theory_assert_axiom(t, mk_binary_app(c, td->Compare, arg1, arg2));
                        break;
                    case Z3_L_FALSE:
                        // assert axiom: !Compare(arg1, arg2)
                        Z3_theory_assert_axiom(t, Z3_mk_not(c, mk_binary_app(c, td->Compare, arg1, arg2)));
                        break;
                    case Z3_L_UNDEF:
                        // do nothing
                        break; 
                    }
                }
            }
        }
        n_prime = Z3_theory_get_eqc_next(t, n_prime);
    }
    while (n_prime != n);
}

/**
   \brief Callback: new_eq for PATheory
*/
void PATh_new_eq(Z3_theory t, Z3_ast n1, Z3_ast n2) {
    PATheoryData * td = (PATheoryData*)Z3_theory_get_ext_data(t);
    Z3_context c = Z3_theory_get_context(t);
    Z3_ast v1 = get_eqc_value(t, n1);
    Z3_ast v2 = get_eqc_value(t, n2);
    display_new_eq(t, n1, n2);
    if (get_eqc_value(t, n1) != 0) {
        apply_compare_axiom_for_parents_of(t, n2, v1);
    }
    if (v2 != 0) {
        apply_compare_axiom_for_parents_of(t, n1, v2);
    }
}

/**
   \brief Callback: final_check for PATheory
*/
Z3_bool PATh_final_check(Z3_theory t) {
    PATheoryData * td = (PATheoryData*)Z3_theory_get_ext_data(t);
    Z3_context c = Z3_theory_get_context(t);
    unsigned i, num;
    printf("Final check\n");
    /* check whether all (relevant) Compare(n1, n2) applications could be evaluated */
    num = Z3_theory_get_num_apps(t);
    for (i = 0; i < num; i++) {
        Z3_ast curr    = Z3_theory_get_app(t, i);
        Z3_func_decl d = Z3_get_app_decl(c, Z3_to_app(c, curr));
        if (d == td->Compare) {
            Z3_ast arg1 = Z3_get_app_arg(c, Z3_to_app(c, curr), 0);
            Z3_ast arg2 = Z3_get_app_arg(c, Z3_to_app(c, curr), 1);
            if (get_eqc_value(t, arg1) == 0 || get_eqc_value(t, arg2) == 0) {
                printf("failed to evaluate Compare(%s", Z3_ast_to_string(c, arg1));
                printf(", %s)\n", Z3_ast_to_string(c, arg2));
                return Z3_FALSE; /* giving up... could not evaluate this Compare application */
            }
        }
    }
    return Z3_TRUE;
}

/**
   \brief Procedural attachment theory example.
*/
Z3_theory mk_pa_theory(Z3_context ctx) {
    Z3_sort compare_domain[2];
    Z3_symbol string_name  = Z3_mk_string_symbol(ctx, "String");
    Z3_symbol compare_name = Z3_mk_string_symbol(ctx, "Compare");
    Z3_sort B              = Z3_mk_bool_sort(ctx);
    PATheoryData * td      = (PATheoryData *)malloc(sizeof(PATheoryData));
    Z3_theory Th           = Z3_mk_theory(ctx, "CompareProcedurealAttachment", td);
    td->String             = Z3_theory_mk_sort(ctx, Th, string_name); 
    compare_domain[0] = td->String; 
    compare_domain[1] = td->String;
    td->Compare            = Z3_theory_mk_func_decl(ctx, Th, compare_name, 2, compare_domain, B);

    Z3_set_delete_callback(Th, PATh_delete);
    Z3_set_reduce_app_callback(Th, PATh_reduce_app);
    Z3_set_new_eq_callback(Th, PATh_new_eq);
    Z3_set_final_check_callback(Th, PATh_final_check);
    return Th;
}

/**
   \brief Test Procedural Attachment Theory Simplifier.
*/
void pa_theory_example1() 
{
    Z3_ast hello, world, c, d, n, r;
    Z3_context ctx;
    Z3_theory Th;
    PATheoryData * td;
    printf("\nprocedural attachment example1\n");
    ctx = mk_context();
    Th = mk_pa_theory(ctx);
    td = (PATheoryData*)Z3_theory_get_ext_data(Th);
    hello = PATh_mk_string_value(Th, "hello");
    world = PATh_mk_string_value(Th, "world");
    c  = mk_var(ctx, "c", td->String);
    d  = mk_var(ctx, "d", td->String);
    n  = Z3_mk_ite(ctx, mk_binary_app(ctx, td->Compare, hello, world), c, d);
    printf("%s\n==>\n", Z3_ast_to_string(ctx, n));
    r  = Z3_simplify(ctx, n);
    printf("%s\n",      Z3_ast_to_string(ctx, r));
    Z3_del_context(ctx);
}


/**
   \brief Test Procedural Attachment Theory.
   
   If assert_b_eq_hello != 0, then we also assert the atom "b = hello".
   
*/
void pa_theory_example2(int assert_b_eq_hello) 
{
    Z3_ast hello, world, test, a, b;
    Z3_ast args[2];
    Z3_context ctx;
    Z3_theory Th;
    PATheoryData * td;
    printf("\nprocedural attachment example2\n");
    ctx = mk_context();
    Th = mk_pa_theory(ctx);
    td = (PATheoryData*)Z3_theory_get_ext_data(Th);
    hello = PATh_mk_string_value(Th, "hello");
    world = PATh_mk_string_value(Th, "world");
    test  = PATh_mk_string_value(Th, "test");
    a     = mk_var(ctx, "a", td->String);
    b     = mk_var(ctx, "b", td->String);
    /* assert a = world \/ a = test */
    args[0] = Z3_mk_eq(ctx, a, world);
    args[1] = Z3_mk_eq(ctx, a, test);
    Z3_assert_cnstr(ctx, Z3_mk_or(ctx, 2, args));
    if (assert_b_eq_hello != 0) {
        /* assert b = hello */
        Z3_assert_cnstr(ctx, Z3_mk_eq(ctx, b, hello));
    }
    /* assert Compare(a, b) */
    Z3_assert_cnstr(ctx, mk_binary_app(ctx, td->Compare, a, b));
    
    if (assert_b_eq_hello != 0) {
        check(ctx, Z3_L_FALSE);
    }
    else {
        /* when "b = hello" is not asserted, the theory solver will
           fail to evaluate Compare(a, b)  */
        check(ctx, Z3_L_UNDEF);
    }
    Z3_del_context(ctx);
}


/*@}*/

/*@{*/

/**
   \brief Simple example to display model.
*/

void model_display_example() {
    Z3_ast x, y, z, c1, c2;
    Z3_ast args[2];
    Z3_sort s;
    Z3_context ctx;
    Z3_theory th;
    Z3_symbol c1_sym, c2_sym;
    printf("\nmodel_display_example\n");
    ctx = mk_context();
    th = Z3_mk_theory(ctx, "Alice And Bob", 0);
    s = Z3_theory_mk_sort(ctx, th, Z3_mk_string_symbol(ctx, "Name"));
    x  = mk_var(ctx, "x", s);
    y  = mk_var(ctx, "y", s);
    z  = mk_var(ctx, "z", s);

    c1_sym = Z3_mk_string_symbol(ctx, "Alice");
    c2_sym = Z3_mk_string_symbol(ctx, "Bob");        
    c1 = Z3_theory_mk_value(ctx, th, c1_sym, s);
    c2 = Z3_theory_mk_value(ctx, th, c2_sym, s);
    args[0] = Z3_mk_eq(ctx, x, c1);
    args[1] = Z3_mk_eq(ctx, x, c2);
    /* asserting x = c1 || x = c2 */
    Z3_assert_cnstr(ctx, Z3_mk_or(ctx, 2, args));
    /* asserting y = z */
    Z3_assert_cnstr(ctx, Z3_mk_eq(ctx, y, z));
    check(ctx, Z3_L_TRUE);
    Z3_del_context(ctx);
}

/*@}*/


/*@}*/

int main() 
{
    simple_example1(); 
    simple_example2();
    pa_theory_example1();
    pa_theory_example2(1); 
    pa_theory_example2(0); 
    model_display_example();
    return 0;
}
