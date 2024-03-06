# Patina
A programming language focusing on gradual memory management


```Rust

fn main() {
    let unique_node = ~Node.new("unique")
    let shared_node = @Node.new("shared")

    consume_unique(unique_node)  // `unique_node` is destroyed immediately
    consume_unique(unique_node)  // 

    consume_shared(shared_node)  // `shared_node` is still available
    consume_shared(shared_node)  // `shared_node` is still available
}


fn consume_unique(node: ~Node) {
    println("{node.val}")
}

fn consume_shared(node: @Node) {
    println("{node.val}")
}

struct Node {
	val: str
}





struct List<E> {
    // An array of E owned by List<E>
	elements: ~[E]
	len: usize
}

fn example() -> List<@Node> {
	let unique_node = Node.new("unique")
	let shared_node = Node.new("shared")
	
	let list_a = List.of(~unique_node)
	let list_b = List.of(@shared_node)
	let list_c = List.of(@shared_node)
	
	return list_c
}


```
