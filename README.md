QxSL: Amateur-Radio Logging Library & LISP
====

![image](https://img.shields.io/badge/Gradle-7-red.svg)
![image](https://img.shields.io/badge/OpenJDK-SE11-red.svg)
![image](https://img.shields.io/badge/GraalVM-20.0-red.svg)
![image](https://img.shields.io/badge/license-LGPL3-darkblue.svg)
![build](https://github.com/autodyne/qxsl/actions/workflows/build.yaml/badge.svg)

QxSL is a Java library for processing amateur-radio log files, including scoring and tabulation frameworks for ham radio contests.

## Features

- provides log en/decoders for QXML, [ADIF](https://adif.org), [Cabrillo](https://wwrof.org/cabrillo), [zLog](https://zlog.org), [CTESTWIN](http://e.gmobb.jp/ctestwin/Download.html), [HAMLOG](http://www.hamlog.com), etc.
- provides tabulation & scoring frameworks for amateur-radio contests and awards.
- provides the rulekit framework which allows you to write contest rules in JS, Ruby, Python, and LISP.

## Documents

- [Javadoc](https://autodyne.github.io/qxsl/docs)
- [ATS-4 (PDF)](https://nextzlog.dev/ats4.pdf)

## Applications

- [ZyLO](https://github.com/nextzlog/zylo).
- [ATS-4](https://github.com/nextzlog/ats4) for [ALLJA1](https://ja1zlo.u-tokyo.org/allja1).

## Sample Codes

Because we are [Scalalians](https://www.scala-lang.org), please be patient to read Scala codes!

### Document Model

The package `qxsl.model` defines the structure of log files, where each communication is handled as an `Item` object, while the entire log is represented as `List[Item]`.
Each `Item` contains some `Field` objects, which indicate properties such as `Time`, `Mode` and `Band`.
In addition, each `Item` holds two `Node` objects, namely `Rcvd` and `Sent`, which involve some messages (`Field`s) exchanged by the operator and the contacted station.

```Scala
import qxsl.model.{Item, Rcvd, Sent}
val item: Item = new Item
val rcvd: Rcvd = item.getRcvd
val sent: Sent = item.getSent
```

### Field Management

The package `qxsl.field` provides a management framework for `Field` implementations.
The class `FieldManager` detects `FieldFactory` implementations [from the class path automatically](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/ServiceLoader.html), and each `FieldFactory` provides en/decoders for individual `Field` implementation.

```Scala
val fields = new qxsl.field.FieldManager()
item.set(fields.cache(new QName("qxsl.org", "mode")).field("CW"))
item.set(fields.cache(new QName("adif.org", "MODE")).field("CW"))
val mode = item.value(new QName("qxsl.org", "mode"))
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
The class `TableManager` detects individual formats (`TableFactory`s) from the class path automatically, and also provides the `decode` method for automatic format detection.

```Scala
val tables = new qxsl.table.TableManager()
val table: java.util.List[Item] = tables.decode(Files.readAllBytes(path))
tables.factory("qxml").encoder(Files.newOutputStream(path)).encode(table)
```

The package `qxsl.sheet` provides an en/decoding framework similar to the `qxsl.table` package, except that `qxsl.sheet` handles contest summary sheets such as Cabrillo and [JARL summary sheet](https://www.jarl.org/Japanese/1_Tanoshimo/1-1_Contest/e-log.htm) R2.0.
The class `SheetManager` manages individual `SheetFactory` implementations.
The class `SheetOrTable` provides the `unpack` method useful for extracting table items from a summary sheet.

```Scala
val sheets = new qxsl.sheet.SheetOrTable()
val table: java.util.List[Item] = sheets.unpack(Files.readAllBytes(path))
```

### Scoring for Awards & Contests

The package `qxsl.ruler` provides a rulemaking framework for amateur radio awards and contests.
Each contest is represented as a `Contest` object, which is provided by a `RuleKit` object or a script engine to define the contest.

```Scala
import qxsl.ruler.{Contest, RuleKit}
val engine = RuleKit.forName("elva")
val stream = engine.getClass().getResourceAsStream("allja1.lisp")
val allja1 = engine.eval(new InputStreamReader(stream)).contest()
```

Each contest involves multiple `Section` objects.
The `Section` object accepts `List[Item]` and validates the communications one by one, by invoking the `summarize` method.

```Scala
import qxsl.ruler.{Section, Summary}
for(section: Section <- allja1.asScala) {
  val summary: Summary = section.summarize(table)
  println(section.code())
  println(section.name())
  summary.accepted().asScala.foreach(println)
  summary.rejected().asScala.foreach(println)
  println(summary.score())
  println(summary.total())
}
```

Currently, the `RuleKit` class supports some domain specific languages, including JS, Ruby, Python, and [Elva](ELVA.md).
Elva is a special LISP for the purpose of contest definition.

### Bundled Contest Definitions

The following LISP programs are bundled inside the JAR file, as sample codes in Elva.

- [`allja1.lisp`](src/main/resources/qxsl/ruler/allja1.lisp)
- [`online.lisp`](src/main/resources/qxsl/ruler/online.lisp)
- [`format.lisp`](src/main/resources/qxsl/ruler/format.lisp)
- [`jautil.lisp`](src/main/resources/qxsl/ruler/jautil.lisp)

## Supported Formats

### TableFactory

| type | format | ext   |
|------|--------|-------|
|`qxml`|QXML    |       |
|`item`|QxSL    |       |
|`jarl`|JARL LOG|       |
|`jswl`|JARL SWL|       |
|`adis`|ADIF    |`.adi` |
|`adxs`|ADIF    |`.adx` |
|`cqww`|CQWW    |`.cbr` |
|`zbin`|zLog    |`.zlo` |
|`znew`|zLog    |`.zlox`|
|`zdos`|zLog    |`.txt` |
|`zall`|zLog    |`.all` |
|`hbin`|HAMLOG  |`.hdb` |
|`cbin`|CTESTWIN|`.lg8` |
|`ctxt`|CTESTWIN|`.txt` |

### SheetFactory

| type | format  |
|------|---------|
|`jarl`|JARL R2.0|
|`cab3`|Cabrillo3|

## Maven

If you want to use the latest build, configure the `build.gradle` as follows:

```Groovy:build.gradle
repositories.maven {
  url('https://autodyne.github.io/qxsl/mvn/')
}

dependencies {
  implementation('qxsl:qxsl:+')
}
```

## Build

[Gradle](https://gradle.org) retrieves dependent libraries, runs tests, and generates a JAR file automatically.

```shell
$ gradle build javadoc publish
```

You can create a [native command line application](https://github.com/autodyne/qxsl/releases/tag/nightly) instead of a JAR file.
[GraalVM](https://www.graalvm.org) must be installed before compilation.
Then run the commands manually as follows:

```shell
$ native-image -jar build/libs/qxsl.jar
$ qxsl.exe source.zlox target.qxml qxml
```

## Contribution

Pull requests are not accepted.
QxSL is a highly extensible library and in most cases QxSL itself does not need to be modified.
If you need enhancements, feel free to make issues at [nextzlog/todo](https://github.com/nextzlog/todo).

## License

### Author

[無線部開発班](https://nextzlog.dev)

### Clauses

- This library is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

- This library is distributed in the hope that it will be useful, but **without any warranty**; without even the implied warranty of **merchantability or fitness for a particular purpose**.
See the GNU Lesser General Public License for more details.

- You should have received a copy of the GNU Lesser General Public License along with this library.
If not, see <http://www.gnu.org/licenses/>.
