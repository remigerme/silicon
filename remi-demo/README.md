# Some artifacts accompanying Rémi Germe's internship report

## Benchmark

On [router.vpr](./router.vpr) (SCION) and [message-lemmas.vpr](./message-lemmas.gobra.vpr) (ReusableVerificationLibrary) - citations in the report.
See [bench.sh](./bench.sh) and [bench-results-router.md](./bench-results-router.md) and [bench-results-message.md](./bench-results-message.md).

## Prusti demo files

Several Viper files outputed by Prusti were manually rewritten to showcase the use of attached facts:

- [bst_generics.rs](./bst_generics_paper.rs_bst_generics_paper--Tree---openang-attached-facts.vpr) ([original file](https://github.com/viperproject/prusti-dev/blob/0d4a8d497ac1e540d48ef50a031d53b4e3ae36e2/prusti-tests/tests/verify_overflow/pass/nfm22/bst_generics_paper.rs))
- [take_inc_max.rs](./take_inc_max.rs_take_inc_max--take_max-Both.vpr) ([original file](https://github.com/viperproject/prusti-dev/blob/0d4a8d497ac1e540d48ef50a031d53b4e3ae36e2/prusti-tests/tests/verify_overflow/pass/simple-specs/take_inc_max.rs))
- [pledges-basic-2.rs](./pledges-basic-2.rs_pledges_basic_2--Nonsense--m3_mut-Bot.vpr) ([original file](https://github.com/viperproject/prusti-dev/blob/9957f85fa614424004ea936e57a7d6f0abfdd007/prusti-tests/tests/verify/pass/erdinm/pledges-basic-2.rs))

```shell
java -Xss32m -jar ./target/scala-2.13/silicon.jar --assumeInjectivityOnInhale --exhaleMode=1 --conditionalizePermissions --includeMethods=resolveLocalDst_163f8674_PMDataPlane,addEndhostPort_163f8674_F ../report/assets/router.vpr > out-router 2>&1
```
