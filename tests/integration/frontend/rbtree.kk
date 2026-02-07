
enum [value] Color {
    Red,
    Black
}

enum Tree<T : Num> {
    Branch(Color, Tree<T>, T, Tree<T>),
    Leaf
}

fn balance_left<T : Num>(l : Tree<T>, k : T, r : Tree<T>) -> Tree<T> {
    match l {
        Tree::Branch(_, Tree::Branch(Color::Red {}, lx, kx, rx), ky, ry) =>
            Tree::Branch{
                Color::Red {},
                Tree::Branch{Color::Black {}, lx, kx, rx},
                ky,
                Tree::Branch{Color::Black {}, ry, k, r}
            },

        Tree::Branch(_, ly, ky, Tree::Branch(Color::Red {}, lx, kx, rx)) =>
            Tree::Branch{
                Color::Red {},
                Tree::Branch{Color::Black {}, ly, ky, lx},
                kx,
                Tree::Branch{Color::Black {}, rx, k, r}
            },

        Tree::Branch(_, lx, kx, rx) =>
            Tree::Branch{
                Color::Black {},
                Tree::Branch{Color::Red {}, lx, kx, rx},
                k,
                r
            },

        Tree::Leaf => Tree::Leaf {}
    }
}


