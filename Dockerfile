FROM java:8
COPY target/bomberman-0.1.0-SNAPSHOT-standalone.jar /bomberman.jar
CMD ["java", "-jar", "/bomberman.jar"]
