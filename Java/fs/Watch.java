package fs;

import java.nio.file.*;
import static java.nio.file.StandardWatchEventKinds.*;
import java.util.*;

public class Watch {
    public static void main(String[] argv) {
        try {
            WatchService serv = FileSystems.getDefault().newWatchService();
            Vector<Path> paths = new Vector<Path>();
            for (String s : argv) {
                Path p = Paths.get(s);
                paths.add(p);
                if (Files.isDirectory(p)) {
                    p.register(serv, ENTRY_MODIFY);
                } else {
                    p = p.toAbsolutePath();
                    p.getParent().register(serv, ENTRY_MODIFY);
                }
            }

            if (paths.isEmpty()) {
                return;
            }

            while (true) {
                final WatchKey key = serv.take();
                for (WatchEvent<?> ev : key.pollEvents()) {
                    final Path changed = (Path)ev.context();
                    paths.stream().anyMatch((Path p) -> changed.endsWith(p));
                    System.out.format("Event: %s on %s\n", ev.kind(), changed.toUri());
                }
                key.reset();
            }
        } catch (Exception e) {
            System.out.format("Error: %s\n", e);
        }
    }
}
