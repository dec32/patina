#[test]
fn test() {
    use crate::compile;
    let src = "
        struct Hello {
            a: @World, b: u8
            b: u16
            c: bool
        }
        const C:i32 = 42
        static S:f64 = 42.42
        fn main(arg0: str, arg1: str) -> @Foo {
            let a = true;
            let b = 10
            let c: u8 = 256
            let d = 114.514
            let s = Hello { a: World {
                    foo: 1
                }
                b: 123
                c: 123
            }
            a = false
            if 2 + 3 * 5 - 4 / 6 == 0 && 1 != 2 || 2 >= 1 {
                b = 1
            } else if random_bool() {
                b = 2
            } else if false {
                b = 3
            } else {
                b = 4
            }
            loop {
                a = a + 1
                if a {
                    break
                }
            }
        }
    ";
    if let Err(e) = compile(src) {
        println!("{:?}", e)
    }
}