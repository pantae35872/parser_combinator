use parser_combinator::{element, json::table, Parser};

fn main() {
    println!(
        "{:?}",
        table().parse(
            r#"
        {
            aaa: 50,
            bbb: {
                haha: 500,
                str: "bbb",
                "quoted": true,
            },
        }"#
        )
    );
}
