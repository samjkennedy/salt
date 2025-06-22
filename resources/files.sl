extern fopen(*char, *char): *FILE;
extern fclose(*FILE): void;
extern fgetc(*FILE): char;

enum Mode {
    Read, Write
}

struct File {
    f: *FILE
}

open_file(path: String, mode: Mode): ?File {
    if mode == Mode::Read {
        f := &fopen(path.data, "r".data)?;
        return File { f: f };
    }
    f := &fopen(path.data, "w".data)?;
    return File { f: f };
}

close_file(file: File): void {
    fclose(file.f);
}

read_char(file: File): char {
    return fgetc(file.f);
}

main(args: []String): void {
    guard args.len == 2 else {
        println("Please provide one file");
        return;
    }

    f := guard open_file(args[1], Mode::Read) else {
        print("Could not open file '"); print(args[1]); println("'");
        return;
    };

    c := read_char(f);
    for i in 0..690 {
        print(c);
        c = read_char(f);
    }

    close_file(f);
}