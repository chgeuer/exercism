#[derive(Debug)]
pub struct Node {
    s: String,
    number: u32,
}

fn main() {
    let n = create_node();
    n.foo()
}

fn create_node() -> Node {
    let mut n = Node {
        number: 1,
        s: "v1".to_string(),
    };
    n.foo();
    println!("Replace {}", n.update_s("v4".to_string()));
    n
}

impl Node {
    pub fn foo(&self) {
        println!("Hello {:?}", self);
    }

    pub fn update_s(&mut self, s: String) -> String {
        std::mem::replace(&mut self.s, s)
    }
}
