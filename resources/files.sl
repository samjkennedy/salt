enum Mode {
    Read, Write
}

open_file(path: String, mode: Mode): *FILE {
    if mode == Mode::Read {
        return fopen(path.data, "r".data);
    } else {
        return fopen(path.data, "w".data);
    }
    return fopen(path.data, "r".data);
}

close_file(file: *FILE): void {
    fclose(file);
}

main(args: []String): void {
    guard args.len == 2 else {
        print("Please provide one file");
        return;
    }

    f := open_file(args[1], Mode::Read)?;

    print("Opened file successfully!");

    close_file(&f);
}