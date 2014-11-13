#include <value.h>
#include <constant.h>
#include <env.h>

value_t PROC29(value_t_list VALUES) {
value_t VAR129 = lookup( A );
return VAR129;

}

int main() {
value_t VAR130 = { .p = &PROC29 };
value_t VAR131 = constant( 11 );
value_t VAR132 = constant( 22 );
value_t_list VARLIST1 = { VAR131, VAR132 };
value_t VAR133 = VAR130.p(VARLIST1);
;
return VAR133.i;
}

