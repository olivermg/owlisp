#include <stdlib.h>
#include <check.h>

#include "error.h"
#include "frame.h"



START_TEST( test_new_frame )
{
  struct frame* frame = new_frame( NULL );

  ck_assert( NULL != frame );
  ck_assert( NULL == frame->parent );
}
END_TEST


START_TEST( test_new_frame_with_parent )
{
  struct frame* frame1 = new_frame( NULL );
  struct frame* frame2 = new_frame( frame1 );

  ck_assert( NULL != frame2 );
  ck_assert( frame1 == frame2->parent );
}
END_TEST


START_TEST( test_bindings_flat )
{
  const int expected_value = 1337;
  const int frameindex = 0;
  const int varindex = 5;

  struct frame* frame = new_frame( NULL );
  set_binding( frame, frameindex, varindex, expected_value );
  int value = get_binding( frame, frameindex, varindex );

  ck_assert( expected_value == value );
}
END_TEST



START_TEST( test_bindings_deep )
{
  const int expected_value = 1337;
  const int frameindex = 1;
  const int varindex = 0;

  struct frame* frame1 = new_frame( NULL );
  struct frame* frame2 = new_frame( frame1 );
  set_binding( frame2, frameindex, varindex, expected_value );
  int value = get_binding( frame2, frameindex, varindex );

  ck_assert( expected_value == value );
}
END_TEST



Suite* runtime_suite (void)
{
  Suite *s = suite_create( "runtime" );

  /* Core test case */
  TCase *tc_core = tcase_create( "Core" );
  tcase_add_test( tc_core, test_new_frame );
  tcase_add_test( tc_core, test_new_frame_with_parent );
  tcase_add_test( tc_core, test_bindings_flat );
  suite_add_tcase( s, tc_core );

  return s;
}

int main (void)
{
  int number_failed;
  Suite *s = runtime_suite();
  SRunner *sr = srunner_create( s );
  srunner_run_all( sr, CK_VERBOSE );
  number_failed = srunner_ntests_failed( sr );
  srunner_free( sr );

  return ( number_failed == 0 ) ? EXIT_SUCCESS : EXIT_FAILURE;
}
