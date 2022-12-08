use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use util::input_as_str_vec;

fn main() {
    println!("{}", clean_up_disk_space(input_as_str_vec!()));
}

fn find_small_dirs(input: Vec<&str>) -> usize {
    let root = create_file_tree(input);

    size_of_all_small_dirs(root.clone())
}

fn clean_up_disk_space(input: Vec<&str>) -> usize {
    let root = create_file_tree(input);
    let size = root.borrow().size;

    size_of_smallest_deletable_dir(root.clone(), size, size)
}

fn create_file_tree(input: Vec<&str>) -> FileLink {
    let root = File::new(0, None);
    let mut current_dir = root.clone();

    for line in input {
        if line.starts_with("$ cd ") {
            current_dir = match line.trim_start_matches("$ cd ") {
                "/" => root.clone(),
                ".." => current_dir.borrow_mut().parent.as_ref().unwrap().clone(),
                _ => current_dir.borrow_mut().get_or_create(line, current_dir.clone()),
            };

        } else if !line.starts_with("$") && !line.starts_with("dir") {
            let (size, name) = line.split_once(" ").unwrap();

            current_dir.borrow_mut().create(
                name,
                size.parse::<usize>().unwrap(),
                current_dir.clone(),
            );
        }
    }

    calculate_dir_size(root.clone());
    root
}

fn calculate_dir_size(dir: FileLink) {
    let mut size: usize = 0;

    for (_, file) in &dir.borrow().directory {
        if file.borrow().directory.len() > 0 {
            calculate_dir_size(file.clone());
        }

        size += file.borrow().size;
    }

    dir.borrow_mut().size = size;
}

fn size_of_all_small_dirs(dir: FileLink) -> usize {
    let mut size: usize = 0;

    for (_, file) in &dir.borrow().directory {
        if file.borrow().directory.len() > 0 {
            size += size_of_all_small_dirs(file.clone());

            if file.borrow().size <= 100000 {
                size += file.borrow().size;
            }
        }
    }

    size
}

fn size_of_smallest_deletable_dir(dir: FileLink, mut smallest_dir: usize, used_space: usize) -> usize {
    for (_, file) in &dir.borrow().directory {
        if file.borrow().directory.len() > 0 {
            let size = size_of_smallest_deletable_dir(file.clone(), smallest_dir, used_space);

            if size < smallest_dir {
                smallest_dir = size;
            }
        }
    }

    if dir.borrow().size < smallest_dir && used_space - dir.borrow().size < 40_000_000 {
        return dir.borrow().size;
    }

    smallest_dir
}

#[derive(Debug)]
struct File {
    size: usize,
    parent: Option<FileLink>,
    directory: HashMap<String, FileLink>,
}

impl File {
    fn new(size: usize, parent: Option<FileLink>) -> FileLink {
        Rc::new(RefCell::new(File {
            size,
            parent,
            directory: HashMap::new(),
        }))
    }

    fn create(&mut self, name: &str, size: usize, link: FileLink) {
        if !self.directory.contains_key(name) {
            self.directory.insert(name.to_string(), File::new(size, Some(link)));
        }
    }

    fn get_or_create(&mut self, name: &str, link: FileLink) -> FileLink {
        self.directory.entry(name.to_string()).or_insert(File::new(0, Some(link))).clone()
    }
}

type FileLink = Rc<RefCell<File>>;
