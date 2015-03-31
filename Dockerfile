FROM java:8
COPY target/bomberman-0.1.0-SNAPSHOT-standalone.jar /bomberman.jar
EXPOSE 3000
CMD ["java", "-jar", "/bomberman.jar"]
