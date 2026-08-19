| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `java -Xss32m -jar ../../silicon-upstream/target/scala-2.13/silicon.jar --parallelizeBranches --select=findEntriesWithInv_c13ece1f_PMTraceManager,findEntryWithInv_c13ece1f_PMTraceManager,findEvents_c13ece1f_PMTraceManager message-lemmas.gobra.vpr` | 25.433 ± 0.471 | 24.367 | 26.227 | 1.00 |
| `java -Xss32m -jar ../target/scala-2.13/silicon.jar --parallelizeBranches --select=findEntriesWithInv_c13ece1f_PMTraceManager,findEntryWithInv_c13ece1f_PMTraceManager,findEvents_c13ece1f_PMTraceManager message-lemmas.gobra.vpr` | 26.129 ± 0.647 | 25.138 | 27.397 | 1.03 ± 0.03 |
