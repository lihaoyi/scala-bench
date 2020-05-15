import bench.MemoryMain$;
import bench.PerfMain$;

public class Main {
    public static void main(String[] args) {
        PerfMain$.MODULE$.main(args);
        MemoryMain$.MODULE$.main(args);
    }
}