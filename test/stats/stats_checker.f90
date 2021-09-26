program stats_checker

    use test_stats_randu
    use test_stats_randn

    call test_stats_randu_integer
    call test_stats_randu_real
    print *, "*<INFO>* All tests in `test_stats_randu` passed."

    call test_stats_randn_real
    print *, "*<INFO>* All tests in `test_stats_randn` passed."

end program stats_checker