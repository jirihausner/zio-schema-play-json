# zio-schema-play-json

`zio-schema-play-json` seamlessly integrates [zio-schema](https://github.com/zio/zio-schema) with the widely used [Play JSON](https://github.com/playframework/play-json) JSON library developed by Play team.

![CI Badge](https://github.com/jirihausner/zio-schema-play-json/actions/workflows/ci.yml/badge.svg?branch=main) ![Maven Central Version](https://img.shields.io/maven-central/v/io.github.jirihausner/zio-schema-play-json_2.13) [![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://github.com/scala-steward-org/scala-steward) [![Scala.js](https://img.shields.io/badge/platform-Scala.js-brightgreen.svg)](https://www.scala-js.org/) [![ZIO Schema Play JSON](https://img.shields.io/github/stars/jirihausner/zio-schema-play-json?style=social)](https://github.com/jirihausner/zio-schema-play-json)

## Why zio-schema-play-json?

- Perfect for projects that already use Play JSON that want to take advantage of the type-safe schema definitions of `zio-schema`.
- Provides an alternative to [zio-schema-json](https://github.com/zio/zio-schema/tree/main/zio-schema-json), catering to teams already invested in Play JSON ecosystem.
- Makes it easier to gradually migrate to `zio-schema` or incorporate its features into legacy stacks.

## Installation

In order to use this library, you need to add one of the following lines in your `build.sbt` file:

```scala
libraryDependencies += "io.github.jirihausner" %% "zio-schema-play-json"     % "0.1.0" // play-json 3.0.+
libraryDependencies += "io.github.jirihausner" %% "zio-schema-play-json-210" % "0.1.0" // play-json 2.10.+
libraryDependencies += "io.github.jirihausner" %% "zio-schema-play-json-27"  % "0.1.0" // play-json 2.7.+
libraryDependencies += "io.github.jirihausner" %% "zio-schema-play-json-26"  % "0.1.0" // play-json 2.6.+
```

`zio-schema-play-json` also provides reimplementation of [plokhotnyuk's jsoniter-scala Circe booster](https://github.com/plokhotnyuk/jsoniter-scala/tree/master/jsoniter-scala-circe) that improves parsing and serialization performance and provides faster encoding and decoding of numeric and java time Play JSON formats, found in `zio-schema-play-json-jsoniter` module. In order to use it, you need to add one of the following lines in your `build.sbt` file:

```scala
libraryDependencies += "io.github.jirihausner" %% "zio-schema-play-json-jsoniter"     % "0.1.0" // play-json 3.0.+
libraryDependencies += "io.github.jirihausner" %% "zio-schema-play-json-jsoniter-210" % "0.1.0" // play-json 2.10.+
libraryDependencies += "io.github.jirihausner" %% "zio-schema-play-json-jsoniter-27"  % "0.1.0" // play-json 2.7.+
libraryDependencies += "io.github.jirihausner" %% "zio-schema-play-json-jsoniter-26"  % "0.1.0" // play-json 2.6.+
```

## Example

```scala
import play.api.libs.json.Format
import zio.schema.codec.play.json.PlayJsonCodec
import zio.schema.{DeriveSchema, Schema}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen
}

// derive Play JSON format from Schema
import play.api.libs.json.Json
implicit val codec: Format[Person] = PlayJsonCodec.schemaFormat(Person.schema)

Json.parse("""{"name": "John", "age": 30}""").as[Person] // Person("John", 30)
Json.stringify(Json.toJson(Person("Adam", 24))) // {"Adam": 24}

// use existing Play JSON format as BinaryCodec
import zio.schema.codec.play.json.PlayJsonCodec.playJsonBinaryCodec

playJsonBinaryCodec[Person](Json.format[Person]) // zio.schema.codec.BinaryCodec[Person]

// derive Play JSON BinaryCodec from schema
import zio.schema.codec.play.json.PlayJsonCodec.schemaBasedBinaryCodec

schemaBasedBinaryCodec[Person](PlayJsonCodec.Config.default) // zio.schema.codec.BinaryCodec[Person]
```

## Acknowledgements

This library was heavily inspired by [zio-schema-json](https://github.com/zio/zio-schema/tree/main/zio-schema-json). Huge thanks to its original contributors for laying foundational ideas and implementation, which greatly influenced `zio-schema-play-json`.

`zio-schema-play-json-jsoniter` builds upon approaches previously implemented in [jsoniter-scala-circe](https://github.com/plokhotnyuk/jsoniter-scala/tree/master/jsoniter-scala-circe) and [play-json-jsoniter](https://github.com/evolution-gaming/play-json-tools/tree/master/play-json-jsoniter) respectively. The decision to reimplement the above libraries in this project, rather than using `play-json-jsoniter` directly, was driven by the requirement to maintain compatibility with older versions of `play-json`. Once again, many thanks to the original authors.

## Disclaimer

`zio-schema-play-json` is not intended to compete with `zio-schema-json`. Instead, it serves as a complementary option for developers who prefer or already use Play JSON in their stack.

---

Contributions are welcome! If you have suggestions, improvements, or feature requests, feel free to open an issue or a pull request.

