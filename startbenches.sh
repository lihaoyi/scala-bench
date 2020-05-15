echo "bench 2.11"
java -Xms6g -Xmx6g -server -cp bench/target/scala-2.11/bench-assembly-0.1.0-SNAPSHOT.jar Main > scala2.11.log
echo "bench 2.12"
java -Xms6g -Xmx6g -server -cp bench/target/scala-2.12/bench-assembly-0.1.0-SNAPSHOT.jar Main > scala2.12.log
echo "bench 2.13"
java -Xms6g -Xmx6g -server -cp bench/target/scala-2.13/bench-assembly-0.1.0-SNAPSHOT.jar Main > scala2.13.log