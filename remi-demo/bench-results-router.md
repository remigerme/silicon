| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `java -Xss32m -jar ../../silicon-upstream/target/scala-2.13/silicon.jar --parallelizeBranches --assumeInjectivityOnInhale --exhaleMode=1 --conditionalizePermissions --select=resolveLocalDst_163f8674_PMDataPlane,addEndhostPort_163f8674_F router.vpr` | 139.253 ± 21.251 | 123.357 | 173.944 | 1.05 ± 0.17 |
| `java -Xss32m -jar ../target/scala-2.13/silicon.jar --parallelizeBranches --assumeInjectivityOnInhale --exhaleMode=1 --conditionalizePermissions --select=resolveLocalDst_163f8674_PMDataPlane,addEndhostPort_163f8674_F router.vpr` | 132.219 ± 7.593 | 124.566 | 143.658 | 1.00 |
