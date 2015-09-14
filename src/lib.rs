 #![feature(trace_macros)]

 //trace_macros!(true);

#[macro_use]
extern crate nom;

use nom::{Needed,IResult, be_u16};
use nom::{Consumer,ConsumerState};
use nom::IResult::*;
//use nom::Err::*;

//use std::str;
//use std::io::SeekFrom;

pub fn be_u24(i: &[u8]) -> IResult<&[u8], u32> {
  if i.len() < 3 {
    Incomplete(Needed::Size(3))
  } else {
    let res = ((i[0] as u32) << 16) + ((i[1] as u32) << 8) + i[2] as u32;
    Done(&i[3..], res)
  }
}

pub fn le_u24(i: &[u8]) -> IResult<&[u8], u32> {
  if i.len() < 3 {
    Incomplete(Needed::Size(3))
  } else {
    let res = ((i[2] as u32) << 16) + ((i[1] as u32) << 8) + i[0] as u32;
    Done(&i[3..], res)
  }
}

pub fn unsigned_unary32(i: (&[u8], usize)) -> IResult<(&[u8], usize), u32> {
  if i.0.len() < 1 {
    Incomplete(Needed::Size(1))
  } else {
    let mut res = 0u32;
    let mut input = i;
    let mut stop = false;
    while !stop {
        match consume_bit(input) {
            Done(inp, bit) => {
                if bit == 0b1 {
                    stop = false;
                }
                else {
                    res += 1;
                }
                input = inp;
            }
            Error(err) => {
                return Error(err);
            }
            Incomplete(Needed::Size(bytes)) => {
                return Incomplete(Needed::Size((res as usize/8)+bytes));
            }
            Incomplete(Needed::Unknown) => {
                return Incomplete(Needed::Unknown);
            }
        }
    }
    Done(input, res)
  }
}

pub fn unsigned_unary64(i: (&[u8], usize)) -> IResult<(&[u8], usize), u64> {
  if i.0.len() < 1 {
    Incomplete(Needed::Size(1))
  } else {
    let mut res = 0u64;
    let mut input = i;
    let mut stop = false;
    while !stop {
        match consume_bit(input) {
            Done(inp, bit) => {
                if bit == 0b1 {
                    stop = false;
                }
                else {
                    res += 1;
                }
                input = inp;
            }
            Error(err) => {
                return Error(err);
            }
            Incomplete(Needed::Size(bytes)) => {
                return Incomplete(Needed::Size((res/8) as usize + bytes));
            }
            Incomplete(Needed::Unknown) => {
                return Incomplete(Needed::Unknown);
            }
        }
    }
    Done(input, res)
  }
}

fn current_bit(i: (&[u8], usize)) -> u8 {
    i.0[0] >> (7 - i.1)
}

fn consume_bit(i: (&[u8], usize)) -> IResult<(&[u8], usize), u8> {
    if i.0.len() < 1 {
        Incomplete(Needed::Size(1))
    }
    else {
        let bit = i.0[0] >> (7 - i.1);

        let new_index = i.1 + 1 % 8;
        let new_slice = if new_index == 0 { &i.0[1..] } else {i.0};

        Done((new_slice, new_index), bit)
    }

}

#[derive(Debug)]
enum MetadataType {
    StreamInfo,
    Padding,
    Application,
    SeekTable,
    VorbisComment,
    CueSheet,
    Picture,
    Reserved(u8),
    Invalid
}


impl MetadataType {
    fn new(block_type : u8) -> MetadataType {
        match block_type {
            0 => MetadataType::StreamInfo,
            1 => MetadataType::Padding,
            2 => MetadataType::Application,
            3 => MetadataType::SeekTable,
            4 => MetadataType::VorbisComment,
            5 => MetadataType::CueSheet,
            6 => MetadataType::Picture,
            7...126 => MetadataType::Reserved(block_type),
            _ => MetadataType::Invalid
        }
    }
}

#[derive(Debug)]
struct MetadataBox<'a> {
    last : bool,
    block_type : MetadataType,
    content : &'a [u8]
}

#[derive(Debug)]
pub struct StreamInfo {
    block_size_min : u16,
    block_size_max : u16,
    frame_size_min : u32,
    frame_size_max : u32,
    sample_rate : u32,
    channels : u8,
    bits_per_sample : u8,
    total_samples : u64,
    md5 : [u8; 16]
}

#[derive(Debug)]
struct FrameInfo {
    fixed_size : bool,
    block_size : u32,
    sample_rate : Option<u32>,
    channels : u8,
    sample_size : Option<u8>,
    first_sameple : u64
}

#[derive(Debug)]
enum SubframeType {
    Constant,
    Verbatim,
    Fixed(u8),
    Lpc(u8)
}

#[derive(Debug)]
struct SubframeInfo {
    encoding_type : SubframeType,
    wasted_bits : u8
}

named!(utf8_initial_byte_u32<(&[u8], usize),(usize,u32)>,
    chain!(
        byte: take_bits!(u32, 8),
        || {
            if byte & 0b10000000 == 0 {
                (0, byte & 0b01111111)
            }
            else if byte & 0b11100000 == 0b11000000 {
                (1, byte & 0b00011111)
            }
            else if byte & 0b11110000 == 0b11100000 {
                (2, byte & 0b00001111)
            }
            else if byte & 0b11111000 == 0b11110000 {
                (3, byte & 0b00000111)
            }
            else if byte & 0b11111100 == 0b11111000 {
                (4, byte & 0b00000011)
            }
            else if byte & 0b11111110 == 0b11111100 {
                (5, byte & 0b00000001)
            }
            else
            {
                (0, 0xffffffff)
            }
        }
        )
    );

named!(utf8_u32<(&[u8], usize),u32>,
    chain!(
        init_out: utf8_initial_byte_u32 ~
        other_bytes:  count!(take_bits!(u32, 8), init_out.0),
        || {
            if init_out.1 != 0xffffffff {
                let mut value : u32 = init_out.1;
                for byte in other_bytes {
                   if byte & 0x11000000 == 0x10000000 {
                       value <<= 6;
                       value |= byte & 0b00111111
                   }
                   else
                   {
                       return 0xffffffff
                   }
                }
                println!("value: {}", value);
                value
            }
            else
            {
                0xffffffff
            }
        }
        )
    );

    named!(utf8_initial_byte_u64<(&[u8], usize),(usize,u64)>,
        chain!(
            byte: take_bits!(u64, 8),
            || {
                println!("{:b}", byte);
                if byte & 0b10000000 == 0 {
                    println!("0");
                    (0, byte & 0b01111111)
                }
                else if byte & 0b11100000 == 0b11000000 {
                    println!("1");
                    (1, byte & 0b00011111)
                }
                else if byte & 0b11110000 == 0b11100000 {
                    println!("2");
                    (2, byte & 0b00001111)
                }
                else if byte & 0b11111000 == 0b11110000 {
                    println!("3");
                    (3, byte & 0b00000111)
                }
                else if byte & 0b11111100 == 0b11111000 {
                    println!("4");
                    (4, byte & 0b00000011)
                }
                else if byte & 0b11111110 == 0b11111100 {
                    println!("5");
                    (5, byte & 0b00000001)
                }
                else if byte & 0b11111111 == 0b11111110 {
                    println!("6");
                    (6, byte & 0b00000000)
                }
                else
                {
                    (0, 0xffffffffffffffff)
                }
            }
            )
        );

    named!(utf8_u64<(&[u8], usize),u64>,
        chain!(
            init_out: utf8_initial_byte_u64 ~
            other_bytes: count!(take_bits!(u64, 8), init_out.0),
            || {
                if init_out.1 != 0xffffffffffffffff {
                    let mut value : u64 = init_out.1;
                    println!("{}", init_out.0);
                    for byte in other_bytes {
                        if byte & 0x11000000 == 0x10000000 {
                            value <<= 6;
                            value |= byte & 0b00111111
                        }
                        else
                        {
                            return 0xffffffffffffffff
                        }
                    }
                    value
                }
                else
                {
                    0xffffffffffffffff
                }// lolololol rofl =)
            }
            )
        );



named!(magic<&[u8],&[u8]>, tag!(b"fLaC"));

named!(metadata_block<&[u8],MetadataBox>,
    chain!(
        block_type: take!(1) ~
        length: be_u24 ~
        content: take!(length),
        || {
                let block_type = block_type[0];
                MetadataBox { last: block_type & 0x80 != 0,
                    block_type: MetadataType::new(block_type & 0x7f),
                    content: content
                }
           }
        )
    );

named!(metadata_streaminfo<&[u8], StreamInfo>,
    chain!(
        block_size_min: be_u16 ~
        block_size_max: be_u16 ~
        frame_size_min: be_u24 ~
        frame_size_max: be_u24 ~
        packed_info: take!(8) ~
        md5: take!(16),
        || {
            let sample_rate : u32 = ((packed_info[0] as u32) << 12) +
                                    ((packed_info[1] as u32) << 4) +
                                    (((packed_info[2] & 0xF0) as u32) >> 4);
            let channels : u8 = ((packed_info[2] & 0x0E) >> 1) + 1;
            let bits_per_sample : u8 =  (((packed_info[2] & 0x01)as u16) << 4) as u8 +
                                       ((packed_info[3] & 0xF0) >> 4) + 1;
            let total_samples : u64 = (((packed_info[3] & 0x0F) as u64) << 32) +
                                      ((packed_info[4] as u64) << 24) +
                                      ((packed_info[5] as u64) << 16) +
                                      ((packed_info[6] as u64) << 8) +
                                      (packed_info[7] as u64);
            let mut md5_array : [u8; 16] = [0; 16];
            let mut md5_index = 0;
            for b in md5
            {
                md5_array[md5_index] = *b;
                md5_index += 1;
            }
            StreamInfo {
                block_size_min : block_size_min,
                block_size_max : block_size_max,
                frame_size_min : frame_size_min,
                frame_size_max : frame_size_max,
                sample_rate : sample_rate,
                channels : channels,
                bits_per_sample : bits_per_sample,
                total_samples : total_samples,
                md5 : md5_array }
        }
        )
    );

named!(frame_header<&[u8], ()/*FrameHeader*/>,
    bits!(
    chain!(
        sync: take_bits!(u16, 14) ~
        _reserved: take_bits!(u8, 1) ~
        blocking_strategy: take_bits!(u8, 1) ~
        block_size: take_bits!(u8, 4) ~
        sample_rate: take_bits!(u8, 4) ~
        channels: take_bits!(u8, 4) ~
        sample_size: take_bits!(u8, 3) ~
        _reserved2: take_bits!(u8, 1) ~
        sample_number: cond!(blocking_strategy == 0x01, utf8_u64) ~
        frame_number: cond!(blocking_strategy == 0x00, utf8_u32) ~
        block_size_var: cond!(block_size & 0b1110 == 0b0110, take_bits!(u16, if block_size & 0b0001 == 0b0001 {16} else {8} ))~
        frame_number_var: cond!(sample_rate != 0b1111 && sample_rate & 0b1100 == 0b1100, take_bits!(u16, if sample_rate & 0b0011 == 0b0000 {8} else {16}))~
        crc : take_bits!(u8, 8),
        || {
            println!("sync: {:b}  blocking_strat: {}  block_size: {:b} sample_rate: {:b} channels: {:b} sample_size:{:b} sample_number: {:?} frame_number: {:?} block_size_var: {:?} frame_number_var: {:?} crc: {:b}",
             sync, blocking_strategy, block_size, sample_rate, channels, sample_size, sample_number, frame_number, block_size_var, frame_number_var, crc);
        }
        ))
    );

named!(subframe_header<&[u8], ()>,
    bits!(
        chain!(
            _padding: take_bits!(u8, 1) ~
            subframe_type: take_bits!(u8, 6) ~
            wasted_bits_flag: take_bits!(u8, 1) ~
            num_wasted_bits: cond!(wasted_bits_flag == 0x01, unsigned_unary32),
            || {
                println!("type {:b} wasted_bits {:b} num_wasted_bits {:?}", subframe_type, wasted_bits_flag, num_wasted_bits);
            }
            )
        )
    );

enum FLACState {
    Init,
    MetadataHeader,
    FrameHeader,
    SubframeHeader,
    Subframe,
    FrameFooter
}

pub struct FLACConsumer {
    state : FLACState,
    stream_info : Option<StreamInfo>
}

impl FLACConsumer {
    pub fn new() -> FLACConsumer {
        FLACConsumer { state: FLACState::Init, stream_info: None }
    }

    fn consume_init(&mut self, input : &[u8]) -> ConsumerState {
        match magic(input) {
            Done(_, _) => {
                println!("Seen Magic Number");
                self.state = FLACState::MetadataHeader;
                ConsumerState::Await(4, input.len()-4)
            }
            Incomplete(_) => {
                ConsumerState::Await(0, 4)
            }
            _ => {
                println!("Not the beginning of a flac file {}", input.len());
                ConsumerState::ConsumerError(0)
            }
        }
    }

    fn consume_metadata(&mut self, input : &[u8]) -> ConsumerState {
        match metadata_block(input) {
            Done(_, b) => {
                println!("Found metadata, type {:?}, length {:?}", b.block_type, b.content.len());
                if  let MetadataType::StreamInfo = b.block_type
                {
                    match metadata_streaminfo(b.content) {
                        Done(_, info) => {
                            self.stream_info = Some(info);
                            if let Some(ref info) = self.stream_info {
                                println!("Found Streaminfo {:?}", info);
                            }
                        }
                        _ => {}
                    }
                }
                if b.last {
                    println!("Last Meta block");
                    self.state = FLACState::FrameHeader;
                }
                ConsumerState::Await(b.content.len()+4, input.len()-b.content.len()-4)
            }
            Incomplete(a) => {
                if let Needed::Size(extra) = a {
                    ConsumerState::Await(0, input.len()+extra)
                }
                else
                {
                    ConsumerState::Await(0, input.len()+100)
                }
            }
            _ => {
                println!("Not metadata!");
                ConsumerState::ConsumerError(0)
            }
        }
    }

    fn consume_frame_header(&mut self, input : &[u8]) -> ConsumerState {
        match frame_header(input) {
            Done(i, ()) => {
                println!( "Header");
                self.state = FLACState::SubframeHeader;
                return ConsumerState::Await(input.len() - i.len(), i.len());
            }
            Incomplete(a) => {
                if let Needed::Size(extra) = a {
                    ConsumerState::Await(0, input.len()+extra)
                }
                else
                {
                    ConsumerState::Await(0, input.len()+100)
                }
            }
            _ => {
                println!("Not Frame Header!");
                ConsumerState::ConsumerError(0)
            }
        }
    }

    fn consume_subframe_header(&mut self, input : &[u8]) -> ConsumerState {
        match subframe_header(input) {
            Done(i, ()) => {
                return ConsumerState::ConsumerDone;
            }
            Incomplete(a) => {
                if let Needed::Size(extra) = a {
                    ConsumerState::Await(0, input.len()+extra)
                }
                else
                {
                    ConsumerState::Await(0, input.len()+100)
                }
            }
            _ => {
                println!("Not a subframe!");
                ConsumerState::ConsumerError(0)
            }
        }
    }
}

impl Consumer for FLACConsumer {
    fn consume(&mut self, input: &[u8])-> ConsumerState {
        match self.state {
            FLACState::Init => {
                self.consume_init(input)
            }
            FLACState::MetadataHeader => {
                self.consume_metadata(input)
            }
            FLACState::FrameHeader => {
                self.consume_frame_header(input)
            }
            FLACState::SubframeHeader => {
                self.consume_subframe_header(input)
            }
            _ => {
                ConsumerState::ConsumerError(0)
            }
        }
    }

    fn failed(&mut self, error_code: u32) {
        println!("failed with error code: {}", error_code);
    }

    fn end(&mut self){
        println!("finish!");
    }
}
