#[derive(Debug)]
pub struct Node<T> {
    data: T,
    next: Next<T>,
    len: usize,
}

#[derive(Debug)]
pub struct SimpleLinkedList<T> {
    head: Next<T>,
}

type Next<T> = Option<Box<Node<T>>>;

impl<T> SimpleLinkedList<T> {
    pub fn new() -> Self {
        SimpleLinkedList { head: None }
    }

    pub fn push(&mut self, element: T) {
        let data: T = element;
        let next: Next<T> = self.head.take();
        let len: usize = match &next {
            None => 1,
            Some(x) => x.len + 1,
        };
        self.head = Some(Box::new(Node { data, next, len }));
    }

    pub fn pop(&mut self) -> Option<T> {
        match self.head {
            None => None,
            Some(_) => {
                let old_head: Box<Node<T>> = self.head.take().unwrap();
                self.head = old_head.next;
                Some(old_head.data)
            }
        }
    }

    pub fn peek(&self) -> Option<&T> {
        match &self.head {
            None => None,
            Some(x) => Some(&x.data),
        }
    }

    pub fn len(&self) -> usize {
        match &self.head {
            None => 0,
            Some(x) => x.len,
        }
    }
}

impl<T: Clone> SimpleLinkedList<T> {
    pub fn rev(&self) -> SimpleLinkedList<T> {
        let mut reversed: SimpleLinkedList<T> = SimpleLinkedList::new();
        let mut next: &Next<T> = &self.head;
        let mut finished = false;
        while !finished {
            match next {
                None => {
                    finished = true;
                }
                Some(x) => {
                    reversed.push(x.data.clone());
                    next = &x.next;
                }
            }
        }

        reversed
    }
}

impl<'a, T: Clone> From<&'a [T]> for SimpleLinkedList<T> {
    fn from(item: &[T]) -> Self {
        let mut result: SimpleLinkedList<T> = SimpleLinkedList::new();
        for x in item.iter() {
            result.push(x.clone());
        }
        result
    }
}

impl<T> Into<Vec<T>> for SimpleLinkedList<T> {
    fn into(self) -> Vec<T> {
        let mut result: Vec<T> = Vec::with_capacity(self.len());
        let mut next: Next<T> = self.head;
        let mut finished = false;
        while !finished {
            match next {
                None => {
                    finished = true;
                }
                Some(x) => {
                    result.push(x.data);
                    next = x.next;
                }
            }
        }
        result.reverse();
        result
    }
}