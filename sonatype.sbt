// Following instructions from https://github.com/xerial/sbt-sonatype
// see https://issues.sonatype.org/browse/OSSRH-27720
pomExtra in Global :=
    <inceptionYear>2017</inceptionYear>
    <scm>
      <url>git@github.com:Chymyst/curryhoward.git</url>
      <connection>scm:git:git@github.com:Chymyst/curryhoward.git</connection>
    </scm>
    <developers>
      <developer>
        <id>winitzki</id>
        <name>Sergei Winitzki</name>
        <url>https://sites.google.com/site/winitzki</url>
      </developer>
    </developers>

sonatypeProfileName := "winitzki"
