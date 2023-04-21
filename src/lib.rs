#[macro_use]
extern crate nom;

use nom::{be_u16, le_u16, le_u32, le_u8};
use std::io::Write;

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

impl Header {
    pub fn to_writer<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        writer.write_all(&[0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00])?;

        let manufacturer_id: u16 = ((self.vendor[2] as u8 + 1 - b'A') as u16)
            | ((self.vendor[1] as u8 + 1 - b'A') as u16) << 5
            | ((self.vendor[0] as u8 + 1 - b'A') as u16) << 10;
        // the manufacturer ID is a legacy plug and play ID, in big-endian
        writer.write_all(&manufacturer_id.to_be_bytes())?;
        writer.write_all(&self.product.to_le_bytes())?;
        writer.write_all(&self.serial.to_le_bytes())?;
        writer.write_all(&[self.week])?;
        writer.write_all(&[self.year])?;
        writer.write_all(&[self.version])?;
        writer.write_all(&[self.revision])
    }
}

fn parse_vendor(v: u16) -> [char; 3] {
    let mask: u8 = 0x1F; // Each letter is 5 bits
    let i0 = (b'A') - 1; // 0x01 = A
    [
        (((v >> 10) as u8 & mask) + i0) as char,
        (((v >> 5) as u8 & mask) + i0) as char,
        ((v as u8 & mask) + i0) as char,
    ]
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

impl Display {
    pub fn to_writer<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        writer.write_all(&[self.video_input])?;
        writer.write_all(&[self.width])?;
        writer.write_all(&[self.height])?;
        writer.write_all(&[self.gamma])?;
        writer.write_all(&[self.features])
    }
}

named!(parse_display<&[u8], Display>, do_parse!(
    video_input: le_u8
    >> width: le_u8
    >> height: le_u8
    >> gamma: le_u8
    >> features: le_u8
    >> (Display{video_input, width, height, gamma, features})
));

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Interlace {
    Interlaced,
    NonInterlaced
}

impl From<u8> for Interlace {
    fn from(val: u8) -> Self {
        if val & 0b1000_0000 > 0 {
            Interlace::Interlaced
        } else {
            Interlace::NonInterlaced
        }
    }
}

impl Interlace {
    fn into_u8(self) -> u8 {
        match self {
            Interlace::Interlaced       => 1 << 7,
            Interlace::NonInterlaced    => 0
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum StereoMode {
    NonStereo,
    FieldSequentialRightWhenSync,
    FieldSequentialLeftWhenSync,
    TwoWayInterleavedRightOnEvenLines,
    TwoWayInterleavedLeftOnEvenLines,
    FourWayInterleaved,
    SideBySide
}

impl From<u8> for StereoMode {
    fn from(val: u8) -> Self {
        let bit0 = val & 0b0000_0001;
        let stereobits = (val & 0b0110_0000) >> 4 | bit0;
        match stereobits {
            0b010 => StereoMode::FieldSequentialRightWhenSync,
            0b100 => StereoMode::FieldSequentialLeftWhenSync,
            0b011 => StereoMode::TwoWayInterleavedRightOnEvenLines,
            0b101 => StereoMode::TwoWayInterleavedLeftOnEvenLines,
            0b110 => StereoMode::FourWayInterleaved,
            0b111 => StereoMode::SideBySide,
            _ => StereoMode::NonStereo,
        }
    }
}

impl StereoMode {
    fn into_u8(self) -> u8 {
        let bit65;
        let bit0;
        match self {
            StereoMode::NonStereo => { bit65 = 0; bit0 = 0; },
            StereoMode::FieldSequentialRightWhenSync => { bit65 = 0b01 << 5; bit0 = 0; },
            StereoMode::FieldSequentialLeftWhenSync => { bit65 = 0b10 << 5; bit0 = 0; },
            StereoMode::TwoWayInterleavedRightOnEvenLines => { bit65 = 0b01 << 5; bit0 = 1; },
            StereoMode::TwoWayInterleavedLeftOnEvenLines => { bit65 = 0b10 << 5; bit0 = 1; },
            StereoMode::FourWayInterleaved => { bit65 = 0b11 << 5; bit0 = 0; },
            StereoMode::SideBySide => { bit65 = 0b11 << 5; bit0 = 0; }
        }

        bit65 | bit0
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Serrate {
    HsyncDuringVsync,
    Without
}

impl From<u8> for Serrate {
    fn from(val: u8) -> Self {
        if val & 0b0100 > 0 {
            Serrate::HsyncDuringVsync
        } else {
            Serrate::Without
        }
    }
}

impl Serrate {
    fn into_u8(self) -> u8 {
        match self {
            Serrate::HsyncDuringVsync  => 1 << 2,
            Serrate::Without           => 0
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum OnRgb {
    RgbSync,
    GreenSync
}

impl From<u8> for OnRgb {
    fn from(val: u8) -> Self {
        if val & 0b0010 > 0 {
            OnRgb::RgbSync
        } else {
            OnRgb::GreenSync
        }
    }
}

impl OnRgb {
    fn into_u8(self) -> u8 {
        match self {
            OnRgb::RgbSync     => 1 << 1,
            OnRgb::GreenSync   => 0
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum CompositePolarity {
    Positive,
    Negative
}

impl From<u8> for CompositePolarity {
    fn from(val: u8) -> Self {
        if val & 0b0010 > 0 {
            CompositePolarity::Positive
        } else {
            CompositePolarity::Negative
        }
    }
}

impl CompositePolarity {
    fn into_u8(self) -> u8 {
        match self {
            CompositePolarity::Positive => 1 << 1,
            CompositePolarity::Negative => 0
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum VerticalSyncPolarity {
    Positive,
    Negative
}

impl From<u8> for VerticalSyncPolarity {
    fn from(val: u8) -> Self {
        if val & 0b0100 > 0 {
            VerticalSyncPolarity::Positive
        } else {
            VerticalSyncPolarity::Negative
        }
    }
}

impl VerticalSyncPolarity {
    fn into_u8(self) -> u8 {
        match self {
            VerticalSyncPolarity::Positive => 1 << 2,
            VerticalSyncPolarity::Negative => 0
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum HorizontalSyncPolarity {
    Positive,
    Negative
}

impl From<u8> for HorizontalSyncPolarity {
    fn from(val: u8) -> Self {
        if val & 0b0010 > 0 {
            HorizontalSyncPolarity::Positive
        } else {
            HorizontalSyncPolarity::Negative
        }
    }
}

impl HorizontalSyncPolarity {
    fn into_u8(self) -> u8 {
        match self {
            HorizontalSyncPolarity::Positive => 1 << 1,
            HorizontalSyncPolarity::Negative => 0
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum SyncType {
    AnalogComposite(Serrate, OnRgb),
    BipolarAnalogComposite(Serrate, OnRgb),
    DigitalComposite(Serrate, CompositePolarity),
    DigitalSeparate(VerticalSyncPolarity, HorizontalSyncPolarity)
}

impl From<u8> for SyncType {
    fn from(val: u8) -> Self {
        match (val & 0b0001_1000) >> 3 {
            0b01 => {
                SyncType::BipolarAnalogComposite(Serrate::from(val), OnRgb::from(val))
            },
            0b10 => {
                SyncType::DigitalComposite(Serrate::from(val), CompositePolarity::from(val))
            },
            0b11 => {
                SyncType::DigitalSeparate(VerticalSyncPolarity::from(val), HorizontalSyncPolarity::from(val))
            },
            _ => {
                SyncType::AnalogComposite(Serrate::from(val), OnRgb::from(val))
            }
        }
    }
}

impl SyncType {
    fn into_u8(self) -> u8 {
        match self {
            SyncType::AnalogComposite(s, o) => s.into_u8() | o.into_u8(),
            SyncType::BipolarAnalogComposite(s, o) => 0b01 << 3 | s.into_u8() | o.into_u8(),
            SyncType::DigitalComposite(s, c) => 0b10 << 3 | s.into_u8() | c.into_u8(),
            SyncType::DigitalSeparate(v, h) => 0b11 << 3 | v.into_u8() | h.into_u8(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct DetailedTimingFeatures {
    pub interlace: Interlace,
    pub stereomode: StereoMode,
    pub synctype: SyncType
}

impl From<u8> for DetailedTimingFeatures {
    fn from(val: u8) -> Self {
        DetailedTimingFeatures{ interlace: Interlace::from(val), stereomode: StereoMode::from(val), synctype: SyncType::from(val) }
    }
}

impl DetailedTimingFeatures {
    fn into_u8(self) -> u8 {
        self.interlace.into_u8() | self.stereomode.into_u8() | self.synctype.into_u8()
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
/// Detailed timing descriptor. All tuples contain the
/// horizontal and vertical component.
pub struct DetailedTiming {
    /// Pixel clock in kHz.
    pub pixel_clock: u32,
    pub active: (u16, u16),
    pub blanking: (u16, u16),
    pub front_porch: (u16, u16),
    pub sync: (u16, u16),
    /// Size in millimeters
    pub size: (u16, u16),
    /// Border pixels on one side of screen (i.e. total number is twice this)
    pub border_pixels: (u8, u8),
    pub features: DetailedTimingFeatures,
}

impl DetailedTiming {
    pub fn to_writer<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        writer.write_all(&((self.pixel_clock / 10) as u16).to_le_bytes())?;
        writer.write_all(&[self.active.0 as u8])?;
        writer.write_all(&[self.blanking.0 as u8])?;

        let byte4 = ((self.active.0 & 0xf00) >> 4) as u8 | ((self.blanking.0 & 0xf00) >> 8) as u8;
        writer.write_all(&[byte4])?;

        writer.write_all(&[self.active.1 as u8])?;
        writer.write_all(&[self.blanking.1 as u8])?;

        let byte7 = ((self.active.1 & 0xf00) >> 4) as u8 | ((self.blanking.1 & 0xf00) >> 8) as u8;
        writer.write_all(&[byte7])?;

        writer.write_all(&[self.front_porch.0 as u8])?;
        writer.write_all(&[self.sync.0 as u8])?;

        let byte10 = ((self.front_porch.1 & 0x0f) << 4) as u8 | (self.sync.1 & 0x0f) as u8;
        writer.write_all(&[byte10])?;

        let byte11 = ((self.front_porch.0 & 0x300) >> 2) as u8
            | ((self.sync.0 & 0x300) >> 4) as u8
            | ((self.front_porch.1 & 0x300) >> 6) as u8
            | ((self.sync.1 & 0x300) >> 8) as u8;
        writer.write_all(&[byte11])?;

        writer.write_all(&[self.size.0 as u8])?;
        writer.write_all(&[self.size.1 as u8])?;

        let byte14 = ((self.size.0 & 0xf00) >> 4) as u8 | ((self.size.1 & 0xf00) >> 8) as u8;
        writer.write_all(&[byte14])?;

        writer.write_all(&[self.border_pixels.0])?;
        writer.write_all(&[self.border_pixels.1])?;
        writer.write_all(&[self.features.into_u8()])
    }
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
        active: (
            (horizontal_active_lo as u16) | (((horizontal_px_hi >> 4) as u16) << 8),
            (vertical_active_lo as u16) | (((vertical_px_hi >> 4) as u16) << 8),
        ),
        blanking: (
            (horizontal_blanking_lo as u16) | (((horizontal_px_hi & 0xf) as u16) << 8),
            (vertical_blanking_lo as u16) | (((vertical_px_hi & 0xf) as u16) << 8),
        ),
        front_porch: (
            (horizontal_front_porch_lo as u16) | (((porch_sync_hi >> 6) as u16) << 8),
            ((vertical_lo >> 4) as u16) | ((((porch_sync_hi >> 2) & 0x3) as u16) << 8),
        ),
        sync: (
            (horizontal_sync_width_lo as u16) | ((((porch_sync_hi >> 4) & 0x3) as u16) << 8),
            ((vertical_lo & 0xf) as u16) | (((porch_sync_hi & 0x3) as u16) << 8),
        ),
        size: (
            (horizontal_size_lo as u16) | (((size_hi >> 4) as u16) << 8),
            (vertical_size_lo as u16) | (((size_hi & 0xf) as u16) << 8),
        ),
        border_pixels: (horizontal_border, vertical_border),
        features: DetailedTimingFeatures::from(features),
    })
));

#[derive(Debug)]
pub enum DescriptorTextError {
    TooLong(usize),
    InvalidCharacter(char),
}

impl std::error::Error for DescriptorTextError {}

impl std::fmt::Display for DescriptorTextError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TooLong(l) => write!(f, "text is too long: max 13 characters, got {}", l),
            Self::InvalidCharacter(c) => write!(f, "invalid character for CP437: {}", c),
        }
    }
}

/// This contains a CP437 string found in the descriptors section.
/// Its maximum length is 13 characters.
#[derive(Debug, PartialEq, Clone)]
pub struct DescriptorText(Vec<u8>);

impl DescriptorText {
    const MAX_CHARS: usize = 13;

    pub fn new(string: &str) -> Result<Self, DescriptorTextError> {
        let len = string.chars().count();
        if len > Self::MAX_CHARS {
            return Err(DescriptorTextError::TooLong(len));
        }

        let mut bytes = Vec::with_capacity(13);
        for c in string.chars() {
            match cp437::codepoint(c) {
                Some(b) => bytes.push(b),
                None => return Err(DescriptorTextError::InvalidCharacter(c)),
            }
        }

        Ok(Self(bytes))
    }

    pub fn inner(&self) -> &[u8] {
        &self.0
    }

    pub fn into_inner(self) -> Vec<u8> {
        self.0
    }

    pub fn to_writer<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        writer.write_all(&self.0)?;

        let len = self.0.len();

        if len == Self::MAX_CHARS {
            Ok(())
        } else {
            writer.write_all(&[0x0A])?;
            for _ in self.0.len() + 1..Self::MAX_CHARS {
                writer.write_all(&[0x20])?;
            }
            Ok(())
        }
    }
}

impl std::fmt::Display for DescriptorText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|b| cp437::forward(*b))
                .collect::<String>()
        )
    }
}

named!(parse_descriptor_text<&[u8], DescriptorText>,
    map!(
        map!(take!(13), |b| {
            b.iter()
            .filter(|c| **c != 0x0A && **c != 0x20)
            .cloned()
            .collect::<Vec<u8>>()
        }),
        DescriptorText
    )
);

#[derive(Debug, PartialEq, Clone)]
pub enum Descriptor {
    DetailedTiming(DetailedTiming),
    SerialNumber(DescriptorText),
    UnspecifiedText(DescriptorText),
    RangeLimits([u8; 14]), // TODO
    ProductName(DescriptorText),
    WhitePoint([u8; 10]),         // TODO
    StandardTiming([u8; 12]),     // TODO
    ColorManagement([u8; 13]),    // TODO
    TimingCodes([u8; 13]),        // TODO
    EstablishedTimings([u8; 13]), // TODO
    Dummy,
    Unknown { descriptor_type: u8, data: [u8; 13] },
}

impl Descriptor {
    pub fn to_writer<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        match self {
            Descriptor::DetailedTiming(dt) => dt.to_writer(writer),
            Descriptor::SerialNumber(sn) => {
                writer.write_all(&[0x00, 0x00, 0x00, 0xff, 0x00])?;
                sn.to_writer(writer)
            }
            Descriptor::UnspecifiedText(ut) => {
                writer.write_all(&[0x00, 0x00, 0x00, 0xfe, 0x00])?;
                ut.to_writer(writer)
            }
            Descriptor::RangeLimits(d) => {
                writer.write_all(&[0x00, 0x00, 0x00, 0xfd])?;
                writer.write_all(d)
            }
            Descriptor::ProductName(pn) => {
                writer.write_all(&[0x00, 0x00, 0x00, 0xfc, 0x00])?;
                pn.to_writer(writer)
            }
            Descriptor::WhitePoint(d) => {
                writer.write_all(&[0x00, 0x00, 0x00, 0xfb, 0x00])?;
                writer.write_all(d)?;
                writer.write_all(&[0x0A, 0x20, 0x20])
            }
            Descriptor::StandardTiming(d) => {
                writer.write_all(&[0x00, 0x00, 0x00, 0xfa, 0x00])?;
                writer.write_all(d)?;
                writer.write_all(&[0x0A])
            }
            Descriptor::ColorManagement(d) => {
                writer.write_all(&[0x00, 0x00, 0x00, 0xf9, 0x00])?;
                writer.write_all(d)
            }
            Descriptor::TimingCodes(d) => {
                writer.write_all(&[0x00, 0x00, 0x00, 0xf8, 0x00])?;
                writer.write_all(d)
            }
            Descriptor::EstablishedTimings(d) => {
                writer.write_all(&[0x00, 0x00, 0x00, 0xf7, 0x00])?;
                writer.write_all(d)
            }
            Descriptor::Dummy => writer.write_all(&[
                0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00,
            ]),
            Descriptor::Unknown {
                descriptor_type,
                data,
            } => {
                writer.write_all(&[0x00, 0x00, 0x00, *descriptor_type, 0x00])?;
                writer.write_all(data)
            }
        }
    }
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
                    data: count_fixed!(u8, le_u8, 14)
                    >> (Descriptor::RangeLimits(data))
                ) |
                0xFC => do_parse!(
                    take!(1)
                    >> s: parse_descriptor_text
                    >> (Descriptor::ProductName(s))
                ) |
                0xFB => do_parse!(
                    take!(1)
                    >> data: count_fixed!(u8, le_u8, 10)
                    >> take!(3)
                    >> (Descriptor::WhitePoint(data))
                ) |
                0xFA => do_parse!(
                    take!(1)
                    >> data: count_fixed!(u8, le_u8, 12)
                    >> take!(1)
                    >> (Descriptor::StandardTiming(data))
                ) |
                0xF9 => do_parse!(
                    take!(1)
                    >> data: count_fixed!(u8, le_u8, 13)
                    >> (Descriptor::ColorManagement(data))
                ) |
                0xF8 => do_parse!(
                    take!(1)
                    >> data: count_fixed!(u8, le_u8, 13)
                    >> (Descriptor::TimingCodes(data))
                ) |
                0xF7 => do_parse!(
                    take!(1)
                    >> data: count_fixed!(u8, le_u8, 13)
                    >> (Descriptor::EstablishedTimings(data))
                ) |
                0x10 => do_parse!(
                    take!(14)
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

#[derive(Debug)]
pub struct DescriptorsError(usize);

impl std::error::Error for DescriptorsError {}

impl std::fmt::Display for DescriptorsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "expected 4 descriptors, got {}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone)]
/// Wraps a vector of `Descriptor`s to make sure bounds set
/// by the EDID specification are kept. Exactly four `Descriptor`s
/// are expected.
pub struct Descriptors(Vec<Descriptor>);

impl Descriptors {
    const NUM_DESCRIPTORS: usize = 4;

    pub fn new(descriptors: Vec<Descriptor>) -> Result<Self, DescriptorsError> {
        let len = descriptors.len();

        if len != Self::NUM_DESCRIPTORS {
            return Err(DescriptorsError(len));
        }

        Ok(Self(descriptors))
    }

    pub fn inner(&self) -> &[Descriptor] {
        &self.0
    }

    pub fn into_inner(self) -> Vec<Descriptor> {
        self.0
    }

    pub fn to_writer<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        for d in &self.0 {
            d.to_writer(writer)?;
        }

        Ok(())
    }
}

named!(parse_descriptors<&[u8], Descriptors>,
    map!(count!(parse_descriptor, 4),
        Descriptors
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

impl ChromaticityCoordinates {
    pub fn to_writer<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        let rg_lsbs = (((self.red.0 & 0x03) << 6)
            | ((self.red.1 & 0x03) << 4)
            | ((self.green.0 & 0x03) << 2)
            | (self.green.1 & 0x03)) as u8;
        writer.write_all(&[rg_lsbs])?;

        let bw_lsbs = (((self.blue.0 & 0x03) << 6)
            | ((self.blue.1 & 0x03) << 4)
            | ((self.white_point.0 & 0x03) << 2)
            | (self.white_point.1 & 0x03)) as u8;
        writer.write_all(&[bw_lsbs])?;

        writer.write_all(&[(self.red.0 >> 2) as u8])?;
        writer.write_all(&[(self.red.1 >> 2) as u8])?;

        writer.write_all(&[(self.green.0 >> 2) as u8])?;
        writer.write_all(&[(self.green.1 >> 2) as u8])?;

        writer.write_all(&[(self.blue.0 >> 2) as u8])?;
        writer.write_all(&[(self.blue.1 >> 2) as u8])?;

        writer.write_all(&[(self.white_point.0 >> 2) as u8])?;
        writer.write_all(&[(self.white_point.1 >> 2) as u8])
    }
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

impl EstablishedTimings {
    pub fn to_writer<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        let mut bytes = [0; 3];
        for et in &self.0 {
            match et {
                EstablishedTiming::H720V400F70 => bytes[0] |= 1 << 7,
                EstablishedTiming::H720V400F88 => bytes[0] |= 1 << 6,
                EstablishedTiming::H640V480F60 => bytes[0] |= 1 << 5,
                EstablishedTiming::H640V480F67 => bytes[0] |= 1 << 4,
                EstablishedTiming::H640V480F72 => bytes[0] |= 1 << 3,
                EstablishedTiming::H640V480F75 => bytes[0] |= 1 << 2,
                EstablishedTiming::H800V600F56 => bytes[0] |= 1 << 1,
                EstablishedTiming::H800V600F60 => bytes[0] |= 1,
                EstablishedTiming::H800V600F72 => bytes[1] |= 1 << 7,
                EstablishedTiming::H800V600F75 => bytes[1] |= 1 << 6,
                EstablishedTiming::H832V624F75 => bytes[1] |= 1 << 5,
                EstablishedTiming::H1024V768F87 => bytes[1] |= 1 << 4,
                EstablishedTiming::H1024V768F60 => bytes[1] |= 1 << 3,
                EstablishedTiming::H1024V768F70 => bytes[1] |= 1 << 2,
                EstablishedTiming::H1024V768F75 => bytes[1] |= 1 << 1,
                EstablishedTiming::H1280V1024F75 => bytes[1] |= 1,
                EstablishedTiming::H1152V870F75 => bytes[2] |= 1 << 7,
                EstablishedTiming::Other(pos) => bytes[2] |= 1 << pos,
            };
        }

        writer.write_all(&bytes)
    }
}

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

#[derive(Debug)]
/// Error type returned when creating a `StandardTiming`.
pub enum StandardTimingError {
    ResolutionOutOfBounds(u16),
    ResolutionNotDivBy8(u16),
    RefreshRateOutOfBounds(u8),
}

impl std::error::Error for StandardTimingError {}

impl std::fmt::Display for StandardTimingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StandardTimingError::ResolutionOutOfBounds(v) => write!(
                f,
                "horizontal resolution out of bounds: {} not in (256..=2288)",
                v
            ),
            StandardTimingError::ResolutionNotDivBy8(v) => {
                write!(f, "horizontal resolution is not divisable by 8: {}", v)
            }
            StandardTimingError::RefreshRateOutOfBounds(v) => {
                write!(f, "refresh rate out of bounds: {} not in (60..=123)", v)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
/// Standard timing information.
pub struct StandardTiming {
    horizontal_resolution: u16,
    aspect_ratio: AspectRatio,
    refresh_rate: u8,
}

impl StandardTiming {
    /// Create a new `StandardTiming` struct.
    pub fn new(
        horizontal_resolution: u16,
        aspect_ratio: AspectRatio,
        refresh_rate: u8,
    ) -> Result<Self, StandardTimingError> {
        if !(256..=2288).contains(&horizontal_resolution) {
            return Err(StandardTimingError::ResolutionOutOfBounds(
                horizontal_resolution,
            ));
        }

        if horizontal_resolution % 8 != 0 {
            return Err(StandardTimingError::ResolutionNotDivBy8(
                horizontal_resolution,
            ));
        }

        if !(61..=123).contains(&refresh_rate) {
            return Err(StandardTimingError::RefreshRateOutOfBounds(refresh_rate));
        }

        Ok(Self {
            horizontal_resolution,
            aspect_ratio,
            refresh_rate,
        })
    }

    pub fn horizontal_resolution(&self) -> u16 {
        self.horizontal_resolution
    }

    pub fn aspect_ratio(&self) -> &AspectRatio {
        &self.aspect_ratio
    }

    pub fn refresh_rate(&self) -> u8 {
        self.refresh_rate
    }

    pub fn to_writer<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        writer.write_all(&[(self.horizontal_resolution / 8 - 31) as u8])?;

        let mut byte2 = match self.aspect_ratio {
            AspectRatio::AR16_10 => 0x00,
            AspectRatio::AR4_3 => 0x40,
            AspectRatio::AR5_4 => 0x80,
            AspectRatio::AR16_9 => 0xC0,
        };
        byte2 |= self.refresh_rate - 60;

        writer.write_all(&[byte2])
    }
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

#[derive(Debug)]
pub struct StandardTimingsError(usize);

impl std::error::Error for StandardTimingsError {}

impl std::fmt::Display for StandardTimingsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "too many timings: max 8, got {}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone)]
/// Wraps a vector of `StandardTiming`s and makes sure bounds
/// set by the EDID specification are kept.
pub struct StandardTimings(Vec<StandardTiming>);

impl StandardTimings {
    const MAX_STANDARD_TIMINGS: usize = 8;

    /// Create a new `StandardTimings` struct from a vector of `StandardTiming`. Returns
    /// an error if the vector contains more than 8 elements.
    pub fn new(standard_timings: Vec<StandardTiming>) -> Result<Self, StandardTimingsError> {
        let len = standard_timings.len();
        if len > Self::MAX_STANDARD_TIMINGS {
            return Err(StandardTimingsError(len));
        }

        Ok(Self(standard_timings))
    }

    pub fn inner(&self) -> &[StandardTiming] {
        &self.0
    }

    pub fn into_inner(self) -> Vec<StandardTiming> {
        self.0
    }

    pub fn to_writer<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        for st in &self.0 {
            st.to_writer(writer)?;
        }

        for _ in 0..Self::MAX_STANDARD_TIMINGS - self.0.len() {
            writer.write_all(&[0x01, 0x01])?;
        }

        Ok(())
    }
}

named!(parse_standard_timings<&[u8], StandardTimings>,
    map!(
        count!(parse_standard_timing, 8),
        |v| StandardTimings(v.into_iter().flatten().collect())
    )
);

#[derive(Debug, PartialEq, Clone)]
pub struct EDID {
    pub header: Header,
    pub display: Display,
    pub chromaticity: ChromaticityCoordinates,
    pub established_timings: EstablishedTimings,
    pub standard_timings: StandardTimings,
    pub descriptors: Descriptors,
}

impl EDID {
    pub fn to_writer<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        self.header.to_writer(writer)?;
        self.display.to_writer(writer)?;
        self.chromaticity.to_writer(writer)?;
        self.established_timings.to_writer(writer)?;
        self.standard_timings.to_writer(writer)?;
        self.descriptors.to_writer(writer)?;
        writer.write_all(&[0x00])?;
        writer.write_all(&[0x00]) // TODO: checksum
    }
}

named!(parse_edid<&[u8], EDID>, do_parse!(
    header: parse_header
    >> display: parse_display
    >> chromaticity: parse_chromaticity_coordinates
    >> established_timings: parse_established_timings
    >> standard_timings: parse_standard_timings
    >> descriptors: parse_descriptors
    >> take!(1) // number of extensions
    >> take!(1) // checksum
    >> (EDID{header, display, chromaticity, established_timings, standard_timings, descriptors})
));

pub fn parse(data: &[u8]) -> Result<EDID, nom::IError<&[u8]>> {
    parse_edid(data).to_full_result()
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::io::Cursor;

    macro_rules! vga_1_edid {
        () => {
            EDID {
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
                standard_timings: StandardTimings(vec![
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
                ]),
                descriptors: Descriptors(vec![
                    Descriptor::DetailedTiming(DetailedTiming {
                        pixel_clock: 146250,
                        active: (1680, 1050),
                        blanking: (560, 39),
                        front_porch: (104, 3),
                        sync: (176, 6),
                        size: (474, 296),
                        border_pixels: (0, 0),
                        features: DetailedTimingFeatures{ interlace: Interlace::NonInterlaced,
                            stereomode: StereoMode::NonStereo,
                            synctype: SyncType::DigitalSeparate(VerticalSyncPolarity::Positive, HorizontalSyncPolarity::Negative)
                        }
                    }),
                    Descriptor::RangeLimits([
                        0x00, 0x38, 0x4B, 0x1E, 0x51, 0x11, 0x00, 0x0A, 0x20, 0x20, 0x20, 0x20,
                        0x20, 0x20,
                    ]),
                    Descriptor::ProductName(DescriptorText::new("SyncMaster").unwrap()),
                    Descriptor::SerialNumber(DescriptorText::new("HS3P701105").unwrap()),
                ]),
            }
        };
    }

    macro_rules! edp_1_edid {
        () => {
            EDID {
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
                standard_timings: StandardTimings(Vec::new()),
                descriptors: Descriptors(vec![
                    Descriptor::DetailedTiming(DetailedTiming {
                        pixel_clock: 138500,
                        active: (1920, 1080),
                        blanking: (160, 31),
                        front_porch: (48, 3),
                        sync: (32, 5),
                        size: (294, 165),
                        border_pixels: (0, 0),
                        features: DetailedTimingFeatures{ interlace: Interlace::NonInterlaced,
                            stereomode: StereoMode::NonStereo,
                            synctype: SyncType::DigitalSeparate(VerticalSyncPolarity::Negative, HorizontalSyncPolarity::Negative)
                        }
                    }),
                    Descriptor::Dummy,
                    Descriptor::UnspecifiedText(DescriptorText::new("DJCP6ÇLQ133M1").unwrap()),
                    Descriptor::Unknown {
                        descriptor_type: 0x00,
                        data: [2, 65, 3, 40, 0, 18, 0, 0, 11, 1, 10, 32, 32],
                    },
                ]),
            }
        };
    }

    fn test(d: &[u8], expected: &EDID) {
        match parse_edid(d) {
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

        let expected = vga_1_edid!();
        test(d, &expected);
    }

    #[test]
    fn test_card0_edp_1() {
        let d = include_bytes!("../testdata/card0-eDP-1");

        let expected = edp_1_edid!();
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

    fn test_standard_timings(d: &[u8], expected: &StandardTimings) {
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

        let expected = StandardTimings(vec![StandardTiming {
            horizontal_resolution: 1920,
            aspect_ratio: AspectRatio::AR16_9,
            refresh_rate: 60,
        }]);

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

        let expected = StandardTimings(Vec::new());

        test_standard_timings(&data, &expected);
    }

    fn test_header_roundtrip(expected: &Header) {
        let mut cursor = Cursor::new(Vec::with_capacity(20));
        expected.to_writer(&mut cursor).unwrap();

        let len = cursor.get_ref().len();
        assert_eq!(len, 20);

        let testee = parse_header(cursor.get_ref()).unwrap().1;

        assert_eq!(&testee, expected);
    }

    #[test]
    fn test_vga_1_header_roundtrip() {
        let expected = vga_1_edid!().header;
        test_header_roundtrip(&expected);
    }

    #[test]
    fn test_edp_1_header_roundtrip() {
        let expected = edp_1_edid!().header;
        test_header_roundtrip(&expected);
    }

    fn test_display_roundtrip(expected: &Display) {
        let mut cursor = Cursor::new(Vec::with_capacity(5));
        expected.to_writer(&mut cursor).unwrap();

        let len = cursor.get_ref().len();
        assert_eq!(len, 5);

        let testee = parse_display(cursor.get_ref()).unwrap().1;

        assert_eq!(&testee, expected);
    }

    #[test]
    fn test_vga_1_display_roundtrip() {
        let expected = vga_1_edid!().display;
        test_display_roundtrip(&expected);
    }

    #[test]
    fn test_edp_1_display_roundtrip() {
        let expected = edp_1_edid!().display;
        test_display_roundtrip(&expected);
    }

    fn test_chromaticity_roundtrip(expected: &ChromaticityCoordinates) {
        let mut cursor = Cursor::new(Vec::with_capacity(10));
        expected.to_writer(&mut cursor).unwrap();

        let len = cursor.get_ref().len();
        assert_eq!(len, 10);

        let testee = parse_chromaticity_coordinates(cursor.get_ref()).unwrap().1;

        assert_eq!(&testee, expected);
    }

    #[test]
    fn test_vga_1_chromaticity_roundtrip() {
        let expected = vga_1_edid!().chromaticity;
        test_chromaticity_roundtrip(&expected);
    }

    #[test]
    fn test_edp_1_chromaticity_roundtrip() {
        let expected = edp_1_edid!().chromaticity;
        test_chromaticity_roundtrip(&expected);
    }

    fn test_est_timings_roundtrip(expected: &EstablishedTimings) {
        let mut cursor = Cursor::new(Vec::with_capacity(3));
        expected.to_writer(&mut cursor).unwrap();

        let len = cursor.get_ref().len();
        assert_eq!(len, 3);

        let testee = parse_established_timings(cursor.get_ref()).unwrap().1;

        assert_eq!(&testee, expected);
    }

    #[test]
    fn test_vga_1_est_timings_roundtrip() {
        let expected = vga_1_edid!().established_timings;
        test_est_timings_roundtrip(&expected);
    }

    #[test]
    fn test_edp_1_est_timings_roundtrip() {
        let expected = edp_1_edid!().established_timings;
        test_est_timings_roundtrip(&expected);
    }

    fn test_standard_timings_roundtrip(expected: &StandardTimings) {
        let mut cursor = Cursor::new(Vec::with_capacity(16));
        expected.to_writer(&mut cursor).unwrap();

        let len = cursor.get_ref().len();
        assert_eq!(len, 16);

        let testee = parse_standard_timings(cursor.get_ref()).unwrap().1;

        assert_eq!(&testee, expected);
    }

    #[test]
    fn test_vga_1_standard_timings_roundtrip() {
        let expected = vga_1_edid!().standard_timings;
        test_standard_timings_roundtrip(&expected);
    }

    #[test]
    fn test_edp_1_standard_timings_roundtrip() {
        let expected = edp_1_edid!().standard_timings;
        test_standard_timings_roundtrip(&expected);
    }

    fn test_descriptors_roundtrip(expected: &Descriptors) {
        let mut cursor = Cursor::new(Vec::with_capacity(72));
        expected.to_writer(&mut cursor).unwrap();

        let len = cursor.get_ref().len();
        assert_eq!(len, 72);

        let testee = parse_descriptors(cursor.get_ref()).unwrap().1;

        assert_eq!(&testee, expected);
    }

    #[test]
    fn test_vga_1_descriptors_roundtrip() {
        let expected = vga_1_edid!().descriptors;
        test_descriptors_roundtrip(&expected);
    }

    #[test]
    fn test_edp_1_descriptors_roundtrip() {
        let expected = vga_1_edid!().descriptors;
        test_descriptors_roundtrip(&expected);
    }

    #[test]
    fn test_vga_1_roundtrip() {
        let expected = vga_1_edid!();

        let mut cursor = Cursor::new(Vec::with_capacity(128));
        expected.to_writer(&mut cursor).unwrap();

        let testee = parse_edid(cursor.get_ref()).unwrap().1;

        assert_eq!(testee, expected);
    }

    #[test]
    fn test_edp_1_roundtrip() {
        let expected = edp_1_edid!();

        let mut cursor = Cursor::new(Vec::with_capacity(128));
        expected.to_writer(&mut cursor).unwrap();

        let testee = parse_edid(cursor.get_ref()).unwrap().1;

        assert_eq!(testee, expected);
    }
}
