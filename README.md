qxsl: Hamradio Logging Library
====

![image](https://img.shields.io/badge/Java-SE11-green.svg)
![image](https://img.shields.io/badge/license-LGPL3-green.svg)

qxsl is a Java Library for Logging & Scoring & Regulation for Amateur-Radio Contests.
qxsl is a vital component of [Automatic Acceptance & Tabulation System (ATS)-4](https://github.com/nextzlog/ats4) for [ALLJA1 contest](http://ja1zlo.u-tokyo.org/allja1).

## Features

- qxsl provides log encoders/decoders for QXML, [ADIF(ADX)](http://adif.org), [Cabrillo](https://wwrof.org/cabrillo/), etc.
- qxsl provides tabulation & scoring framework for contests and awards.
- qxsl provides a LISP engine named **elva**, and contest rules can be described in modern S-expression styles.

## Sample Codes

Because we are [Scalalians](https://www.scala-lang.org/), 
please be patient to read Scala codes!

### Decoding & Encoding

qxsl provides an automatic format detector:

```Scala
import java.nio.file.Files
import java.nio.file.Paths
import java.util.List
import qxsl.model.Item
import qxsl.table.TableFormats

val path = Paths.get("Users", "foo", "allja1.ZLO")
val table: List[Item] = new TableFormats().decode(Files.newInputStream(path))

import scala.collection.JavaConverters._
table.asScala.foreach(System.out.println)
```

To output the data into a file in some format, specify [`TableFormat`](https://pafelog.net/qxsl/qxsl/table/TableFormat.html) explicitly:

```Scala
new TableFormats().getFormat("qxml").encode(Files.newOutputStream(path), table)
```

You can obtain a list of format implementations as follows, using Java [ServiceLoader](https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html) mechanism:

```Scala
new TableFormats().asScala.toList.foreach(System.out.println)
```

### Scoring for Awards & Contests

qxsl provides [the script engine **elva**](https://pafelog.net/qxsl/elva/ElvaScriptEngine.html) and a [rulemaking framework](https://pafelog.net/qxsl/qxsl/ruler/package-summary.html).

```Scala
import qxsl.ruler.RuleKit

val contest: Contest = new RuleKit().eval("""
(contest "CQ AWESOME CONTEST"
  (section "CW 14MHz"  (lambda it (verify it (list CW? 14MHz?))))
  (section "CW 21MHz"  (lambda it (verify it (list CW? 21MHz?))))
  (section "CW 28MHz"  (lambda it (verify it (list CW? 28MHz?))))
  (section "PH 14MHz"  (lambda it (verify it (list PH? 14MHz?))))
  (section "PH 21MHz"  (lambda it (verify it (list PH? 21MHz?))))
  (section "PH 28MHz"  (lambda it (verify it (list PH? 28MHz?)))))
"""
```

qxsl contains [the definition of ALLJA1 contest](src/main/resources/qxsl/ruler/allja1.lisp) as a sample inside the JAR file.
You can try the predefined definition as follows:

```Scala
import qxsl.ruler.Contest // contest
import qxsl.ruler.Section // section provided by contest

val contest: Contest = Contest.defined("allja1.lisp") // src/main/resources/qxsl/ruler/allja1.lisp
val section: Section = contest.getSection("1エリア内 社団 電信電話 オールバンド部門")
```

Then, you may summarize an operation list into a Summary object, which involves scores, accepted and rejected items:

``` Scala
import qxsl.ruler.Summary

val summary: Summary = section.summarize(table) // List[Item]
println(summary.score) // sum of scores for accepted items
println(summary.mults) // multiplication of multipliers
println(summary.total) // is score * mults

summary.accepted.asScala.foreach(println)
summary.rejected.asScala.foreach(println)
```

## Documents

- [Javadoc](https://pafelog.net/qxsl/index.html)
- [History and Usage of ATS-4](https://pafelog.net/ats4.pdf)

## Build

`$ gradle build`

## Contribution

Feel free to contact [@nextzlog](https://twitter.com/nextzlog) on Twitter.

## License

### Author

[無線部開発班](https://pafelog.net)

### Clauses

- This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License(LGPL) as published by the Free Software Foundation (FSF), either version 3 of the License, or (at your option) any later version.

- This program is distributed in the hope that it will be useful, but **without any warranty**; without even the implied warranty of **merchantability or fitness for a particular purpose**.
See the GNU Lesser General Public License for more details.

- You should have received a copy of the GNU General Public License and GNU Lesser General Public License along with this program.
If not, see <http://www.gnu.org/licenses/>.
