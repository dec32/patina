# Patina
A programming language focusing on gradual memory management

Example:

```Rust
fn main() {
    let shared = @Node.new("shared") // create a heap-allocated object
    let unique = ~Node.new("unique") // create a heap-allocated object
    let borrow = &unique             // points to a existing object
    
    consume_borrow(borrow)           // `unique` is still available
    consume_unique(unique)           // `unique` is destroyed immediately
    consume_shared(shared)           // `shared` is still available
    consume_shared(shared)           // `shared` is still available
    return                           // `shared` is destroyed
}

fn consume_shared(node: @Node) {
    println("{node.val}")
}

fn consume_unique(node: ~Node) {
    println("{node.val}")
}

fn consume_borrow(node: &Node) {
    println("{node.val}")
}

struct Node {
    val: str
}
```
