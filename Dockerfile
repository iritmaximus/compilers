FROM sbtscala/scala-sbt:eclipse-temurin-alpine-21.0.8_9_1.12.1_3.8.1 AS builder

WORKDIR /usr/src
COPY . .

WORKDIR /usr/src/compiler

RUN sbt clean assembly

FROM eclipse-temurin:21

WORKDIR /usr/src
COPY --from=builder /usr/src/compiler/target/scala-3.8.1/compiler-assembly-0.1.0-SNAPSHOT.jar compiler.jar
COPY ./compiler ./

CMD ["java", "-jar", "compiler.jar"]
