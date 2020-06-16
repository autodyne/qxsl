qxsl: Amateur-Radio Logging Library & LISP
====

![image](https://img.shields.io/badge/OpenJDK-SE11-red.svg)
![image](https://img.shields.io/badge/GraalVM-20.0-red.svg)
![image](https://img.shields.io/badge/Gradle-5-orange.svg)
![image](https://img.shields.io/badge/license-LGPL3-blue.svg)

qxsl is a Java library for processing amateur-radio log files, including scoring and tabulation frameworks for ham radio contests, which are important components of [Automatic Acceptance & Tabulation System (ATS)](https://github.com/nextzlog/ats4) for [the ALLJA1 contest](http://ja1zlo.u-tokyo.org/allja1).

## Features

- qxsl provides log en/decoders for QXML, [ADIF(ADI/ADX)](http://adif.org), [Cabrillo](https://wwrof.org/cabrillo/), [zLog](http://www.zlog.org), [CTESTWIN](http://e.gmobb.jp/ctestwin/Download.html), etc.
- qxsl provides tabulation & scoring frameworks for amateur-radio contests and awards.
- qxsl provides a LISP engine named *Elva*, and contest rules can be described in modern S-expression styles.

## Sample Codes

Because we are [Scalalians](https://www.scala-lang.org/), please be patient to read Scala codes!

### Document Model

The package `qxsl.model` defines the structure of log files, where each communication is handled as an `Item` object, while the entire log is represented as `List[Item]`.
Each `Item` contains some `Field` objects, which indicate properties such as `Time`, `Mode` and `Band`.
In addition, each `Item` holds two `Exch` objects, namely `Rcvd` and `Sent`, which involve some messages (`Field`s) exchanged by the operator and the contacted station.

```Scala
import qxsl.model.{Item,Rcvd,Sent}
val item: Item = new Item
val rcvd: Rcvd = item.getRcvd
val sent: Sent = item.getSent
```

### Field Management

The package `qxsl.field` provides a management framework for `Field` implementations.
The class `FieldFormats` detects `FieldFormat` implementations [from the class path automatically](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/ServiceLoader.html), and each `FieldFormat` provides en/decoders for individual `Field` implementation.

```Scala
val fmts = new qxsl.field.FieldFormats
item.add(fmts.cache(new QName("qxsl.org", "mode")).field("CW"))
item.add(fmts.cache(new QName("adif.org", "MODE")).field("CW"))
val mode = item.get(new QName("qxsl.org", "mode")).value()
```

This mechanism is utilized for en/decoding the *QXML* format, which is an alternative log format proposed by the qxsl development team.
*QXML* is extensible, and supports namespaces which have been prohibited in the traditional ADIF:

```XML:sample.qxml
<?xml version="1.0" encoding="UTF-8"?>
<list xmlns:qxsl="qxsl.org">
  <item qxsl:time="2017-06-03T16:17:00Z" qxsl:call="QV1DOK" qxsl:band="14000" qxsl:mode="CW">
    <rcvd qxsl:rstq="599" qxsl:code="120103"/>
    <sent qxsl:rstq="599" qxsl:code="100110"/>
  </item>
  <item qxsl:time="2017-06-04T00:01:00Z" qxsl:call="QD1QXB" qxsl:band="21000" qxsl:mode="CW">
    <rcvd qxsl:rstq="599" qxsl:code="110117"/>
    <sent qxsl:rstq="599" qxsl:code="100110"/>
  </item>
</list>
```

### Decoding & Encoding

The package `qxsl.table` provides a basic framework for en/decoding log files including QXML and ADIF.
The class `TableFormats` detects individual formats (`TableFormat`s) from the class path automatically, and also provides the `detect` method for automatic format detection.

```Scala
val fmts = new qxsl.table.TableFormats()
val table: List[Item] = fmts.decode(Files.newInputStream(path))
fmts.forName("qxml").encoder(Files.newOutputStream(path)).encode(table)
```

The package `qxsl.sheet` provides an en/decoding framework similar to the `qxsl.table` package, except that `qxsl.sheet` handles contest summary sheets such as Cabrillo and [JARL summary sheet](https://www.jarl.org/Japanese/1_Tanoshimo/1-1_Contest/e-log.htm) R2.0.
The class `SheetFormats` manages individual `SheetFormat` implementations, and also provides the `unpack` method useful for extracting `List[Item]` from a summary sheet.

```Scala
val fmts = new qxsl.sheet.SheetFormats()
val table: List[Item] = fmts.unpack(Files.newBufferedReader(path))
```

### Scoring for Awards & Contests

The package `qxsl.ruler` provides a rulemaking framework for amateur radio awards and contests.
Each contest is represented as a `Contest` object, which involves multiple `Section` objects.
The `Section` object accepts `List[Item]` and validates the communications one by one, by invoking the `summarize` method.
The class `RuleKit` provides a LISP engine optimized for this process.

```Scala
import qxsl.ruler.{Contest,RuleKit,Section,Summary}

val contest: Contest = RuleKit.load("elva").contest("""
(load "qxsl/ruler/radial.lisp")
(defmacro SUCCESS (tests) (lambda it (success it 1 (qxsl-call it))))
(defmacro scoring (score calls mults) `(* score (length ',mults)))
(setq test (contest "CQ AWESOME CONTEST" scoring))
(SECTION test "CW 14MHz Single OP" "SinCW14" (SUCCESS (CW? 14MHz?)))
(SECTION test "CW 21MHz Single OP" "SinCW21" (SUCCESS (CW? 21MHz?)))
(SECTION test "CW 28MHz Single OP" "SinCW28" (SUCCESS (CW? 28MHz?)))
(SECTION test "PH 14MHz Single OP" "SinPH14" (SUCCESS (PH? 14MHz?)))
(SECTION test "PH 21MHz Single OP" "SinPH21" (SUCCESS (PH? 21MHz?)))
(SECTION test "PH 28MHz Single OP" "SinPH28" (SUCCESS (PH? 28MHz?)))""")

val section: Section = contest.getSection("CW 14MHz SINGLE-OP")
val summary: Summary = section.summarize(table)
summary.accepted.asScala.foreach(println)
summary.rejected.asScala.foreach(println)
```

The original LISP engine is provided by the package `elva`.

### Bundled Contest Definitions

The following LISP programs are bundled inside the JAR file, as sample codes in Elva Lisp.

- [allja1.lisp (ALLJA1 contest definition)](https://github.com/nextzlog/qxsl/tree/master/src/main/resources/qxsl/ruler/allja1.lisp)
- [format.lisp (ADIF-QXSL field converter)](https://github.com/nextzlog/qxsl/tree/master/src/main/resources/qxsl/ruler/format.lisp)

## Documents

- [Javadoc](https://nextzlog.github.io/qxsl/doc/index.html)
- [コンテスト運営を支援する自動集計システム (PDF)](https://pafelog.net/ats4.pdf)

## Maven

If you want to use the latest build, configure the `build.gradle` as follows:

```Groovy:build.gradle
repositories.maven {
  url('https://nextzlog.github.io/qxsl/mvn/')
}

dependencies {
  implementation('qxsl:qxsl:+')
}
```

## Build

[Gradle](https://gradle.org/) retrieves dependent libraries, runs tests, and generates a JAR file automatically.

```shell
$ gradle build javadoc publish
```

You can create a native library instead of a JAR file.
[GraalVM](https://www.graalvm.org) must be installed before compilation.
Then run the command manually as follows:

```shell
$ native-image --shared -cp build/libs/qxsl.jar -H:Name=qxsl
```

You must implement [entry points](https://www.graalvm.org/sdk/javadoc/org/graalvm/nativeimage/c/function/CEntryPoint.html) by your self.

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
