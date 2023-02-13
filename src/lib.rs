#[macro_use]
extern crate nom;

use nom::{be_u16, le_u16, le_u32, le_u8};

mod cp437;

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Header {
    pub vendor: [char; 3],
    pub product: u16,
    pub serial: u32,
    pub week: u8,
    pub year: u8, // Starting at year 1990
    pub version: u8,
    pub revision: u8,
}

fn parse_vendor(v: u16) -> [char; 3] {
    let mask: u8 = 0x1F; // Each letter is 5 bits
    let i0 = ('A' as u8) - 1; // 0x01 = A
    return [
        (((v >> 10) as u8 & mask) + i0) as char,
        (((v >> 5) as u8 & mask) + i0) as char,
        (((v >> 0) as u8 & mask) + i0) as char,
    ];
}

named!(parse_header<&[u8], Header>, do_parse!(
    tag!(&[0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00][..])
    >> vendor: be_u16
    >> product: le_u16
    >> serial: le_u32
    >> week: le_u8
    >> year: le_u8
    >> version: le_u8
    >> revision: le_u8
    >> (Header{vendor: parse_vendor(vendor), product, serial, week, year, version, revision})
));

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Display {
    pub video_input: u8,
    pub width: u8,  // cm
    pub height: u8, // cm
    pub gamma: u8,  // datavalue = (gamma*100)-100 (range 1.00–3.54)
    pub features: u8,
}

named!(parse_display<&[u8], Display>, do_parse!(
    video_input: le_u8
    >> width: le_u8
    >> height: le_u8
    >> gamma: le_u8
    >> features: le_u8
    >> (Display{video_input, width, height, gamma, features})
));

named!(parse_descriptor_text<&[u8], String>,
    map!(
        map!(take!(13), |b| {
            b.iter()
            .filter(|c| **c != 0x0A)
            .map(|b| cp437::forward(*b))
            .collect::<String>()
        }),
        |s| s.trim().to_string()
    )
);

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct DetailedTiming {
    /// Pixel clock in kHz.
    pub pixel_clock: u32,
    pub horizontal_active_pixels: u16,
    pub horizontal_blanking_pixels: u16,
    pub vertical_active_lines: u16,
    pub vertical_blanking_lines: u16,
    pub horizontal_front_porch: u16,
    pub horizontal_sync_width: u16,
    pub vertical_front_porch: u16,
    pub vertical_sync_width: u16,
    /// Horizontal size in millimeters
    pub horizontal_size: u16,
    /// Vertical size in millimeters
    pub vertical_size: u16,
    /// Border pixels on one side of screen (i.e. total number is twice this)
    pub horizontal_border_pixels: u8,
    /// Border pixels on one side of screen (i.e. total number is twice this)
    pub vertical_border_pixels: u8,
    pub features: u8, /* TODO add enums etc. */
}

named!(parse_detailed_timing<&[u8], DetailedTiming>, do_parse!(
    pixel_clock_10khz: le_u16
    >> horizontal_active_lo: le_u8
    >> horizontal_blanking_lo: le_u8
    >> horizontal_px_hi: le_u8
    >> vertical_active_lo: le_u8
    >> vertical_blanking_lo: le_u8
    >> vertical_px_hi: le_u8
    >> horizontal_front_porch_lo: le_u8
    >> horizontal_sync_width_lo: le_u8
    >> vertical_lo: le_u8
    >> porch_sync_hi: le_u8
    >> horizontal_size_lo: le_u8
    >> vertical_size_lo: le_u8
    >> size_hi: le_u8
    >> horizontal_border: le_u8
    >> vertical_border: le_u8
    >> features: le_u8
    >> (DetailedTiming {
        pixel_clock: pixel_clock_10khz as u32 * 10,
        horizontal_active_pixels: (horizontal_active_lo as u16) |
                                  (((horizontal_px_hi >> 4) as u16) << 8),
        horizontal_blanking_pixels: (horizontal_blanking_lo as u16) |
                                    (((horizontal_px_hi & 0xf) as u16) << 8),
        vertical_active_lines: (vertical_active_lo as u16) |
                               (((vertical_px_hi >> 4) as u16) << 8),
        vertical_blanking_lines: (vertical_blanking_lo as u16) |
                                 (((vertical_px_hi & 0xf) as u16) << 8),
        horizontal_front_porch: (horizontal_front_porch_lo as u16) |
                                (((porch_sync_hi >> 6) as u16) << 8),
        horizontal_sync_width: (horizontal_sync_width_lo as u16) |
                               ((((porch_sync_hi >> 4) & 0x3) as u16) << 8),
        vertical_front_porch: ((vertical_lo >> 4) as u16) |
                              ((((porch_sync_hi >> 2) & 0x3) as u16) << 8),
        vertical_sync_width: ((vertical_lo & 0xf) as u16) |
                             (((porch_sync_hi & 0x3) as u16) << 8),
        horizontal_size: (horizontal_size_lo as u16) | (((size_hi >> 4) as u16) << 8),
        vertical_size: (vertical_size_lo as u16) | (((size_hi & 0xf) as u16) << 8),
        horizontal_border_pixels: horizontal_border,
        vertical_border_pixels: vertical_border,
        features: features
    })
));

#[derive(Debug, PartialEq, Clone)]
pub enum Descriptor {
    DetailedTiming(DetailedTiming),
    SerialNumber(String),
    UnspecifiedText(String),
    RangeLimits, // TODO
    ProductName(String),
    WhitePoint,     // TODO
    StandardTiming, // TODO
    ColorManagement,
    TimingCodes,
    EstablishedTimings,
    Dummy,
    Unknown { descriptor_type: u8, data: [u8; 13] },
}

named!(parse_descriptor<&[u8], Descriptor>,
    switch!(peek!(le_u16),
        0 => do_parse!(
            take!(3)
            >> d: switch!(le_u8,
                0xFF => do_parse!(
                    take!(1)
                    >> s: parse_descriptor_text
                    >> (Descriptor::SerialNumber(s))
                ) |
                0xFE => do_parse!(
                    take!(1)
                    >> s: parse_descriptor_text
                    >> (Descriptor::UnspecifiedText(s))
                ) |
                0xFD => do_parse!(
                    take!(1)
                    >> take!(13)
                    >> (Descriptor::RangeLimits)
                ) |
                0xFC => do_parse!(
                    take!(1)
                    >> s: parse_descriptor_text
                    >> (Descriptor::ProductName(s))
                ) |
                0xFB => do_parse!(
                    take!(1)
                    >> take!(13)
                    >> (Descriptor::WhitePoint)
                ) |
                0xFA => do_parse!(
                    take!(1)
                    >> take!(13)
                    >> (Descriptor::StandardTiming)
                ) |
                0xF9 => do_parse!(
                    take!(1)
                    >> take!(13)
                    >> (Descriptor::ColorManagement)
                ) |
                0xF8 => do_parse!(
                    take!(1)
                    >> take!(13)
                    >> (Descriptor::TimingCodes)
                ) |
                0xF7 => do_parse!(
                    take!(1)
                    >> take!(13)
                    >> (Descriptor::EstablishedTimings)
                ) |
                0x10 => do_parse!(
                    take!(1)
                    >> take!(13)
                    >> (Descriptor::Dummy)
                ) |
                dt => do_parse!(
                    take!(1)
                    >> data: count_fixed!(u8, le_u8, 13)
                    >> (Descriptor::Unknown{ descriptor_type: dt, data })
                )
            )
            >> (d)
        ) |
        _ => do_parse!(
            d: parse_detailed_timing
            >> (Descriptor::DetailedTiming(d))
        )
    )
);

#[derive(Debug, PartialEq, Clone)]
/// 10-bit 2° CIE 1931 xy coordinates for red, green, blue, and white point.
pub struct ChromaticityCoordinates {
    red: (u16, u16),
    green: (u16, u16),
    blue: (u16, u16),
    white_point: (u16, u16),
}

named!(parse_chromaticity_coordinates<&[u8], ChromaticityCoordinates>, do_parse!(
    rg_lsbs: le_u8
    >> bw_lsbs: le_u8
    >> rx_msb: le_u8
    >> ry_msb: le_u8
    >> gx_msb: le_u8
    >> gy_msb: le_u8
    >> bx_msb: le_u8
    >> by_msb: le_u8
    >> wx_msb: le_u8
    >> wy_msb: le_u8
    >> (ChromaticityCoordinates {
        red: (((rx_msb as u16) << 2) | ((rg_lsbs & 0xc0) >> 6) as u16, ((ry_msb as u16) << 2) | ((rg_lsbs & 0x30) >> 4) as u16),
        green: (((gx_msb as u16) << 2) | ((rg_lsbs & 0x0c) >> 2) as u16, ((gy_msb as u16) << 2) | (rg_lsbs & 0x03) as u16),
        blue: (((bx_msb as u16) << 2) | ((bw_lsbs & 0xc0) >> 6) as u16, ((by_msb as u16) << 2) | ((bw_lsbs & 0x30) >> 4) as u16),
        white_point: (((wx_msb as u16) << 2) | ((bw_lsbs & 0x0c) >> 2) as u16, ((wy_msb as u16) << 2) | (bw_lsbs & 0x03) as u16)
    })
));

/// Display mode as specified in established timings.
pub struct DisplayMode {
    pub h: u16,
    pub v: u16,
    pub f: u16,
}

/// (Formerly common) established timings.
#[derive(Debug, PartialEq, Clone)]
pub enum EstablishedTiming {
    H720V400F70,
    H720V400F88,
    H640V480F60,
    H640V480F67,
    H640V480F72,
    H640V480F75,
    H800V600F56,
    H800V600F60,
    H800V600F72,
    H800V600F75,
    H832V624F75,
    H1024V768F87,
    H1024V768F60,
    H1024V768F70,
    H1024V768F75,
    H1280V1024F75,
    H1152V870F75,
    /// Manufacturer specific timing. Contains the bit position of the
    /// timing in the lowest 7 bit of the last byte in the established timing
    /// EDID section.
    Other(u8),
}

impl EstablishedTiming {
    /// Returns the corresponding display mode. Returns `None` if the
    /// `EstablishedTiming` is manufacturer specific.
    pub fn as_display_mode(&self) -> Option<DisplayMode> {
        match self {
            EstablishedTiming::H720V400F70 => Some(DisplayMode {
                h: 720,
                v: 400,
                f: 70,
            }),
            EstablishedTiming::H720V400F88 => Some(DisplayMode {
                h: 720,
                v: 400,
                f: 88,
            }),
            EstablishedTiming::H640V480F60 => Some(DisplayMode {
                h: 640,
                v: 480,
                f: 60,
            }),
            EstablishedTiming::H640V480F67 => Some(DisplayMode {
                h: 640,
                v: 480,
                f: 67,
            }),
            EstablishedTiming::H640V480F72 => Some(DisplayMode {
                h: 640,
                v: 480,
                f: 72,
            }),
            EstablishedTiming::H640V480F75 => Some(DisplayMode {
                h: 640,
                v: 480,
                f: 75,
            }),
            EstablishedTiming::H800V600F56 => Some(DisplayMode {
                h: 800,
                v: 600,
                f: 56,
            }),
            EstablishedTiming::H800V600F60 => Some(DisplayMode {
                h: 800,
                v: 600,
                f: 60,
            }),
            EstablishedTiming::H800V600F72 => Some(DisplayMode {
                h: 800,
                v: 600,
                f: 72,
            }),
            EstablishedTiming::H800V600F75 => Some(DisplayMode {
                h: 800,
                v: 600,
                f: 75,
            }),
            EstablishedTiming::H832V624F75 => Some(DisplayMode {
                h: 832,
                v: 624,
                f: 75,
            }),
            EstablishedTiming::H1024V768F87 => Some(DisplayMode {
                h: 1024,
                v: 768,
                f: 87,
            }),
            EstablishedTiming::H1024V768F60 => Some(DisplayMode {
                h: 1024,
                v: 768,
                f: 60,
            }),
            EstablishedTiming::H1024V768F70 => Some(DisplayMode {
                h: 1024,
                v: 768,
                f: 70,
            }),
            EstablishedTiming::H1024V768F75 => Some(DisplayMode {
                h: 1024,
                v: 768,
                f: 75,
            }),
            EstablishedTiming::H1280V1024F75 => Some(DisplayMode {
                h: 1280,
                v: 1024,
                f: 75,
            }),
            EstablishedTiming::H1152V870F75 => Some(DisplayMode {
                h: 1152,
                v: 870,
                f: 75,
            }),
            EstablishedTiming::Other(_) => None,
        }
    }
}

// TODO: this is likely not in the spirit of nom, find a better way
fn parse_established_timings_bytes(i: &[u8]) -> nom::IResult<&[u8], EstablishedTimings> {
    let mut iter = i.iter();
    let mut vec = Vec::new();

    match iter.next() {
        Some(b) => {
            for i in 0..7 {
                let pos = 1 << i;
                if pos & b > 0 {
                    let et = match i {
                        0 => EstablishedTiming::H800V600F60,
                        1 => EstablishedTiming::H800V600F56,
                        2 => EstablishedTiming::H640V480F75,
                        3 => EstablishedTiming::H640V480F72,
                        4 => EstablishedTiming::H640V480F67,
                        5 => EstablishedTiming::H640V480F60,
                        6 => EstablishedTiming::H720V400F88,
                        7 => EstablishedTiming::H720V400F70,
                        _ => unreachable!(),
                    };
                    vec.push(et);
                }
            }
        }
        None => return nom::IResult::Incomplete(nom::Needed::Size(3)),
    }

    match iter.next() {
        Some(b) => {
            for i in 0..7 {
                let pos = 1 << i;
                if pos & b > 0 {
                    let et = match i {
                        0 => EstablishedTiming::H1280V1024F75,
                        1 => EstablishedTiming::H1024V768F75,
                        2 => EstablishedTiming::H1024V768F70,
                        3 => EstablishedTiming::H1024V768F60,
                        4 => EstablishedTiming::H1024V768F87,
                        5 => EstablishedTiming::H832V624F75,
                        6 => EstablishedTiming::H800V600F75,
                        7 => EstablishedTiming::H800V600F72,
                        _ => unreachable!(),
                    };
                    vec.push(et);
                }
            }
        }
        None => return nom::IResult::Incomplete(nom::Needed::Size(2)),
    }

    match iter.next() {
        Some(b) => {
            for i in 0..7 {
                let pos = 1 << i;
                if pos & b > 0 {
                    let et = match i {
                        7 => EstablishedTiming::H1152V870F75,
                        other => EstablishedTiming::Other(other),
                    };
                    vec.push(et);
                }
            }
        }
        None => return nom::IResult::Incomplete(nom::Needed::Size(1)),
    }

    nom::IResult::Done(&i[3..], EstablishedTimings(vec))
}

#[derive(Debug, PartialEq, Clone)]
pub struct EstablishedTimings(pub Vec<EstablishedTiming>);

named!(parse_established_timings<&[u8], EstablishedTimings>, do_parse!(
    v: parse_established_timings_bytes >> (v)
));

#[derive(Debug, PartialEq, Clone)]
/// Image aspect ratio.
pub enum AspectRatio {
    AR16_10,
    AR4_3,
    AR5_4,
    AR16_9,
}

#[derive(Debug, PartialEq, Clone)]
/// Standard timing information.
pub struct StandardTiming {
    pub horizontal_resolution: u16,
    pub aspect_ratio: AspectRatio,
    pub refresh_rate: u8,
}

named!(parse_standard_timing<&[u8], Option<StandardTiming>>,
    do_parse!(
        hr: le_u8
        >> ar_rr: le_u8
        >> r: cond!(hr != 1 || ar_rr != 1,
            // we need to give a type hint here
            add_return_error!(nom::ErrorKind::Custom(0), do_parse!((StandardTiming {
                horizontal_resolution: (hr as u16 + 31) * 8,
                aspect_ratio: match ar_rr >> 6 {
                    0 => AspectRatio::AR16_10,
                    1 => AspectRatio::AR4_3,
                    2 => AspectRatio::AR5_4,
                    3 => AspectRatio::AR16_9,
                    _ => unreachable!(),
                },
                refresh_rate: (ar_rr & 0x3f) + 60,
            })))
        ) >> (r)
    )
);

named!(parse_standard_timings<&[u8], Vec<StandardTiming>>,
    map!(
        count!(parse_standard_timing, 8),
        |v| v.into_iter().flatten().collect()
    )
);

#[derive(Debug, PartialEq, Clone)]
pub struct EDID {
    pub header: Header,
    pub display: Display,
    pub chromaticity: ChromaticityCoordinates,
    pub established_timings: EstablishedTimings,
    pub standard_timings: Vec<StandardTiming>,
    pub descriptors: Vec<Descriptor>,
}

named!(parse_edid<&[u8], EDID>, do_parse!(
    header: parse_header
    >> display: parse_display
    >> chromaticity: parse_chromaticity_coordinates
    >> established_timings: parse_established_timings
    >> standard_timings: parse_standard_timings
    >> descriptors: count!(parse_descriptor, 4)
    >> take!(1) // number of extensions
    >> take!(1) // checksum
    >> (EDID{header, display, chromaticity, established_timings, standard_timings, descriptors})
));

pub fn parse(data: &[u8]) -> nom::IResult<&[u8], EDID> {
    parse_edid(data)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test(d: &[u8], expected: &EDID) {
        match parse(d) {
            nom::IResult::Done(remaining, parsed) => {
                assert_eq!(remaining.len(), 0);
                assert_eq!(&parsed, expected);
            }
            nom::IResult::Error(err) => {
                panic!("{}", err);
            }
            nom::IResult::Incomplete(_) => {
                panic!("Incomplete");
            }
        }
    }

    #[test]
    fn test_card0_vga_1() {
        let d = include_bytes!("../testdata/card0-VGA-1");

        let expected = EDID {
            header: Header {
                vendor: ['S', 'A', 'M'],
                product: 596,
                serial: 1146106418,
                week: 27,
                year: 17,
                version: 1,
                revision: 3,
            },
            display: Display {
                video_input: 14,
                width: 47,
                height: 30,
                gamma: 120,
                features: 42,
            },
            chromaticity: ChromaticityCoordinates {
                red: (659, 341),
                green: (293, 617),
                blue: (156, 81),
                white_point: (321, 337),
            },
            established_timings: EstablishedTimings(vec![
                EstablishedTiming::H800V600F60,
                EstablishedTiming::H800V600F56,
                EstablishedTiming::H640V480F75,
                EstablishedTiming::H640V480F72,
                EstablishedTiming::H640V480F67,
                EstablishedTiming::H640V480F60,
                EstablishedTiming::H1280V1024F75,
                EstablishedTiming::H1024V768F75,
                EstablishedTiming::H1024V768F70,
                EstablishedTiming::H1024V768F60,
                EstablishedTiming::H832V624F75,
                EstablishedTiming::H800V600F75,
            ]),
            standard_timings: vec![
                StandardTiming {
                    horizontal_resolution: 1680,
                    aspect_ratio: AspectRatio::AR16_10,
                    refresh_rate: 60,
                },
                StandardTiming {
                    horizontal_resolution: 1280,
                    aspect_ratio: AspectRatio::AR5_4,
                    refresh_rate: 60,
                },
                StandardTiming {
                    horizontal_resolution: 1280,
                    aspect_ratio: AspectRatio::AR4_3,
                    refresh_rate: 60,
                },
                StandardTiming {
                    horizontal_resolution: 1152,
                    aspect_ratio: AspectRatio::AR4_3,
                    refresh_rate: 75,
                },
            ],
            descriptors: vec![
                Descriptor::DetailedTiming(DetailedTiming {
                    pixel_clock: 146250,
                    horizontal_active_pixels: 1680,
                    horizontal_blanking_pixels: 560,
                    vertical_active_lines: 1050,
                    vertical_blanking_lines: 39,
                    horizontal_front_porch: 104,
                    horizontal_sync_width: 176,
                    vertical_front_porch: 3,
                    vertical_sync_width: 6,
                    horizontal_size: 474,
                    vertical_size: 296,
                    horizontal_border_pixels: 0,
                    vertical_border_pixels: 0,
                    features: 28,
                }),
                Descriptor::RangeLimits,
                Descriptor::ProductName("SyncMaster".to_string()),
                Descriptor::SerialNumber("HS3P701105".to_string()),
            ],
        };

        test(d, &expected);
    }

    #[test]
    fn test_card0_edp_1() {
        let d = include_bytes!("../testdata/card0-eDP-1");

        let expected = EDID {
            header: Header {
                vendor: ['S', 'H', 'P'],
                product: 5193,
                serial: 0,
                week: 32,
                year: 25,
                version: 1,
                revision: 4,
            },
            display: Display {
                video_input: 165,
                width: 29,
                height: 17,
                gamma: 120,
                features: 14,
            },
            chromaticity: ChromaticityCoordinates {
                red: (655, 337),
                green: (307, 614),
                blue: (153, 61),
                white_point: (320, 336),
            },
            established_timings: EstablishedTimings(Vec::new()),
            standard_timings: Vec::new(),
            descriptors: vec![
                Descriptor::DetailedTiming(DetailedTiming {
                    pixel_clock: 138500,
                    horizontal_active_pixels: 1920,
                    horizontal_blanking_pixels: 160,
                    vertical_active_lines: 1080,
                    vertical_blanking_lines: 31,
                    horizontal_front_porch: 48,
                    horizontal_sync_width: 32,
                    vertical_front_porch: 3,
                    vertical_sync_width: 5,
                    horizontal_size: 294,
                    vertical_size: 165,
                    horizontal_border_pixels: 0,
                    vertical_border_pixels: 0,
                    features: 24,
                }),
                Descriptor::Dummy,
                Descriptor::UnspecifiedText("DJCP6ÇLQ133M1".to_string()),
                Descriptor::Unknown {
                    descriptor_type: 0x00,
                    data: [2, 65, 3, 40, 0, 18, 0, 0, 11, 1, 10, 32, 32],
                },
            ],
        };

        test(d, &expected);
    }

    fn test_chromaticity(d: &[u8], expected: &ChromaticityCoordinates) {
        match parse_chromaticity_coordinates(d) {
            nom::IResult::Done(remaining, parsed) => {
                assert_eq!(remaining.len(), 0);
                assert_eq!(&parsed, expected);
            }
            nom::IResult::Error(err) => {
                panic!("{}", err);
            }
            nom::IResult::Incomplete(_) => {
                panic!("Incomplete");
            }
        }
    }

    #[test]
    fn test_chromaticity_coordinates_vga_1() {
        let d: &[u8] = &include_bytes!("../testdata/card0-VGA-1")[25..35];

        let expected = ChromaticityCoordinates {
            red: (659, 341),
            green: (293, 617),
            blue: (156, 81),
            white_point: (321, 337),
        };

        test_chromaticity(d, &expected);
    }

    #[test]
    fn test_chromaticity_coordinates_edp_1() {
        let d: &[u8] = &include_bytes!("../testdata/card0-eDP-1")[25..35];

        let expected = ChromaticityCoordinates {
            red: (655, 337),
            green: (307, 614),
            blue: (153, 61),
            white_point: (320, 336),
        };

        test_chromaticity(d, &expected);
    }

    fn test_standard_timings(d: &[u8], expected: &Vec<StandardTiming>) {
        match parse_standard_timings(d) {
            nom::IResult::Done(remaining, parsed) => {
                assert_eq!(remaining.len(), 0);
                assert_eq!(&parsed, expected);
            }
            nom::IResult::Error(err) => {
                panic!("{}", err);
            }
            nom::IResult::Incomplete(_) => {
                panic!("Incomplete");
            }
        }
    }

    #[test]
    fn test_standard_timings_simple() {
        let data = [
            0xD1, 0xC0, // 1920, 16:9, 60Hz
            0x01, 0x01, // empty
            0x01, 0x01, // empty
            0x01, 0x01, // empty
            0x01, 0x01, // empty
            0x01, 0x01, // empty
            0x01, 0x01, // empty
            0x01, 0x01, // empty
        ];

        let expected = vec![StandardTiming {
            horizontal_resolution: 1920,
            aspect_ratio: AspectRatio::AR16_9,
            refresh_rate: 60,
        }];

        test_standard_timings(&data, &expected);
    }

    #[test]
    fn test_standard_timings_empty() {
        let data = [
            0x01, 0x01, // empty
            0x01, 0x01, // empty
            0x01, 0x01, // empty
            0x01, 0x01, // empty
            0x01, 0x01, // empty
            0x01, 0x01, // empty
            0x01, 0x01, // empty
            0x01, 0x01, // empty
        ];

        let expected = Vec::new();

        test_standard_timings(&data, &expected);
    }
}
