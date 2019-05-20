qxsl: Hamradio Logging Library
====

![image](https://img.shields.io/badge/Java-SE8-green.svg)
![image](https://img.shields.io/badge/license-LGPL3-green.svg)

qxsl is Java Framework for Logging & Tabulation & Rule-Definition.
qxsl also provides a ScriptEngine for a LISP dialect named Elva.

## Simple Demo

Because we are [Scalalians](https://www.scala-lang.org/), 
please be patient to read Scala codes!

### Decoding & Encoding

If you don't know the log format in advance, write as follows:

```Scala
import java.nio.file.Files
import java.nio.file.Paths
import qxsl.table.Tables

val path = Paths.get("Users", "foo", "allja1.ZLO")
val elog = new Tables().decode(Files.newInputStream(path))

import scala.collection.JavaConverters._
elog.asScala.foreach(System.out.println)
```

To specify the format, write as follows:

```Scala
val elog = new Tables().getFormat("zbin").decode(Files.newInputStream(path))
```

To output the log into a file, write as follows:

```Scala
val elog = new Tables().getFormat("qxml").encode(Files.newOutputStream(path), elog)
```

You can obtain a list of formats implemented by qxsl as follows:

```Scala
val fmts = new Tables().asScala.toList
fmts.foreach(println)
```

It should be noted that qxsl can detect [format implementations](https://pafelog.net/qxsl/qxsl/table/TableFormat.html) automatically,
via Java [ServiceLoader](https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html) mechanism.

### Scoring for Awards & Contests


## Documents

[Javadoc](https://pafelog.net/qxsl/index.html)
[PDF](https://pafelog.net/ats4.pdf)

## Build

`$ gradle build`

## Contribution

[無線部開発班 (Journal of Hamradio Informatics)](https://pafelog.net)

## License

- This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License(LGPL) as published by the Free Software Foundation (FSF), either version 3 of the License, or (at your option) any later version.

- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Lesser General Public License for more details.

- You should have received a copy of the GNU General Public License and GNU Lesser General Public License along with this program.
If not, see <http://www.gnu.org/licenses/>.
