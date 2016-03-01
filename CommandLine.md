### Print the working directory (folder).
    pwd

### Listing the contents of a directory.
    ls
    ls <directory>
    ls -l
    ls -a

### Changing directories.
    cd <directory>
    cd <directory>/<subdirectory>      # Relative path.
    cd /<directory>/<subdirectory>     # Absolute path.
    cd ..                              # Go up a level.
    cd                                 # Go to home directory.

### Creating and removing directories.
    mkdir <directory>
    rmdir <directory>

### Moving and renaming files.
    mv <from-file> <to-file>
    mv <file1> <file2> <file3> <directory>

### Copying files.
    cp <from-file> <to-file>

### Deleting files (and directories).
    rm <file>
    rm -r <directory>     # Recursively remove a directory.
    rm -f <file>          # Forcefully remove a file.

### Create a blank file.
    touch <file>

### Creating a new file or editing and existing file with Vim.
    vim <file>

### Wildcard to select several files.
    rm file*      # Deletes all files that begin with "file".

### Getting help on commands.
    man <command>

### See if a program is on your $PATH.
    which <program>

### Print strings.  Useful for getting values of environment variables.
    echo $PATH

### Redirecting output to a file.
    <command> <arguments> > <output-file>

### Viewing files.
    cat <file>        # Print the whole file.
    less <file>       # Scroll through the file.

### Exiting a shell.
    exit

### Tab complete.

You don't have to type long path names or commands.
The tab key will auto-complete things for you.  






