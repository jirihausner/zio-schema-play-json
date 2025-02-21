# zio-schema-play-json

`zio-schema-play-json` seamlessly integrates [zio-schema](https://github.com/zio/zio-schema) with the widely used [Play JSON](https://github.com/playframework/play-json) JSON library developed by Play team.

![CI Badge](https://github.com/jirihausner/zio-schema-play-json/actions/workflows/ci.yml/badge.svg?branch=main) ![Maven Central Version](https://img.shields.io/maven-central/v/io.github.jirihausner/zio-schema-play-json_2.13) [![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://github.com/scala-steward-org/scala-steward) [![ZIO Schema Play JSON](https://img.shields.io/github/stars/jirihausner/zio-schema-play-json?style=social)](https://github.com/jirihausner/zio-schema-play-json)

## Why zio-schema-play-json?

- Perfect for projects that already use Play JSON that want to take advantage of the type-safe schema definitions of `zio-schema`.
- Provides an alternative to [zio-schema-json](https://github.com/zio/zio-schema/tree/main/zio-schema-json), catering to teams already invested in Play JSON ecosystem.
- Makes it easier to gradually migrate to `zio-schema` or incorporate its features into legacy stacks.

## Installation

In order to use this library, we need to add following line in your `build.sbt` file:

```scala
libraryDependencies += "io.github.jirihausner" %% "zio-schema-play-json" % "0.1.0"
```

## Example

TODO

## Acknowledgements

This library was heavily inspired by [zio-schema-json](https://github.com/zio/zio-schema/tree/main/zio-schema-json). Huge thanks to its original contributors for laying foundational ideas and implementation, which greatly influenced `zio-schema-play-json`.

## Disclaimer

`zio-schema-play-json` is not intended to compete with `zio-schema-json`. Instead, it serves as a complementary option for developers who prefer or already use Play JSON in their stack.

---

Contributions are welcome! If you have suggestions, improvements, or feature requests, feel free to open an issue or a pull request.

