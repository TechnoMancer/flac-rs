#![feature(result_expect)]

extern crate flac;
extern crate nom;

use flac::FLACConsumer;
use nom::{FileProducer, Consumer};


fn explore_flac_file(filename: &str) {
    FileProducer::new(filename, 400).map(|producer:FileProducer| {
        println!("file producer created for {}", filename);
        let mut p = producer;
        let mut c = FLACConsumer::new();
        c.run(&mut p);
        }).expect("No such file");
}


fn main() {
    explore_flac_file("./Moscow Youth Cult - Survivasm (Instrumental).flac");
}
