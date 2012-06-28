use Time::HiRes qw(gettimeofday tv_interval);

# @dirs = ("nofib/imag/exp3_8");
@dirs = ("nofib/imag/bernouilli", "nofib/imag/binarytrees", "nofib/imag/digits-of-e1", "nofib/imag/digits-of-e2", "nofib/imag/exp3_8", "nofib/imag/gen-regexps", "nofib/imag/integrate", "nofib/imag/loop", "nofib/imag/nsieve", "nofib/imag/paraffins", "nofib/imag/partial-sums", "nofib/imag/pidigits", "nofib/imag/primes", "nofib/imag/queens", "nofib/imag/recursive", "nofib/imag/rfib-int", "nofib/imag/rfib-integer", "nofib/imag/rfib-double", "nofib/imag/ru_list", "nofib/imag/tak", "nofib/imag/wheel-sieve1", "nofib/imag/wheel-sieve2", "nofib/imag/x2n1", "nofib/real/infer" );


@compilernames = ("uhc -O2 -tbc", "uhc -O2,2 -tbc", "uhc -tC", "ghc", "ghc -O2");


for $dir (@dirs) {

    @compilers = ("uhc -O2 -tbc --cpp --import-path=$dir", "uhc -O2,2 -tbc --cpp --import-path=$dir", "uhc -tC --cpp --import-path=$dir", "ghc --make -cpp -i$dir", "ghc -O2 --make -cpp -i$dir");

    
    for $j (0,1,2,3,4) {
    
        $compiler = $compilers[$j];
    
        printf("%-24s %-15s", $dir, $compilernames[$j]);
    
        system ("rm $dir/*.hi $dir/*.grin $dir/*.core $dir/*.o $dir/Main $dir/Main 2>/dev/null");
        
        $tc0 = [gettimeofday];
        system ("$compiler $dir/Main.hs 1> $dir/comp-output$j 2> $dir/comp-errors$j");
        $tc1 = [gettimeofday];
    
        my $tc = tv_interval $tc0, $tc1;
        printf("Compile %-8g  ", $tc);
    
    
        if (-e "$dir/Main") {
    
            for $n (1, 2, 3) {
            
                $invoer = "in$n";
                $uitvoer = "out$n";
            
                open (INVOER, "$dir/$invoer");
                $args = <INVOER>;
                chomp $args;
                close INVOER;
                
                system ("rm $dir/run-output$j-$n $dir/run-errors$j-$n 2>/dev/null");

                $tr0 = [gettimeofday];
                if (-e "$dir/stdin$n" ) {
                    system ("$dir/Main $args  < $dir/stdin$n  1> $dir/run-output$j-$n 2> $dir/run-errors$j-$n");
                } else {
                    system ("$dir/Main $args  1> $dir/run-output$j-$n 2> $dir/run-errors$j-$n");
                }
                $tr1 = [gettimeofday];
            
                my $tr = tv_interval $tr0, $tr1;
            
                printf("Run%d %-8g ", $n, $tr);
                
                open (INVOER, "$dir/run-errors$j-$n");
                if (<INVOER>) {
                    print "ERROR ";
                }
                else
                {
                    open UITVOER, "$dir/run-output$j-$n";
                    open EXPECT,  "$dir/$uitvoer";
                    $uit = <UITVOER>;
                    $ex  = <EXPECT>;
                    close UITVOER;
                    close EXPECT;
                    
                    if ($uit!=$ex) { print "WRONG "; }
                    else { print "OK    "; }
                }
                close INVOER;
                
            }
        }
        else
        {
                print "Compilation error";
        }
            
        print "\n";
    }
    print "\n";
}
