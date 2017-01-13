/*
 * Copyright 2006 The Android Open Source Project
 *
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 */

extern crate core_graphics;
extern crate core_text;
extern crate core_foundation;
use core_graphics::base::{kCGImageAlphaNoneSkipFirst};
use core_foundation::string::UniChar;
use core_foundation::base::CFIndex;
use core_graphics::color_space::CGColorSpace;
use core_graphics::context::CGContext;
use core_graphics::font::{CGFont, CGGlyph};
use core_graphics::geometry::CGPoint;
use core_text::font;
use std::mem;

#[test]
fn it_works() {
    let k = supports_LCD();
    assert!(k);
}

fn supports_LCD() -> bool {
    let mut cg_context = CGContext::create_bitmap_context(1, 1, 8, 4,
                                                          &CGColorSpace::create_device_rgb(),
                                                          kCGImageAlphaNoneSkipFirst |
                                                          (2 << 12) //kCGBitmapByteOrder32Little
                                                          );
    let ct_font = font::new_from_name("Helvetica", 16.).unwrap();
    cg_context.set_should_smooth_fonts(true);
    cg_context.set_should_antialias(true);
    cg_context.set_allows_font_smoothing(true);
    //cg_context.set_text_drawing_mode(
    cg_context.set_rgb_fill_color(1.0, 1.0, 1.0, 1.0);
    let point = CGPoint {x: -1., y: 0.};
    let characters: [UniChar; 1] = ['|' as UniChar];
    let mut glyphs: [CGGlyph; 1] = [0 as CGGlyph];
    let count: CFIndex = 1;
    ct_font.get_glyphs_for_characters(&characters[0], &mut glyphs[0], count);
    // XXX: it would be nice if we didn't have to clone cg_context here
    ct_font.draw_glyphs(&glyphs, &[point], cg_context.clone());
    let data = cg_context.data();
    let rgb : u32 = unsafe { std::mem::transmute::<[u8; 4], u32>([data[0],data[1],data[2],data[3]]) };
    let r = (rgb >> 16) & 0xFF;
    let g = (rgb >>  8) & 0xFF;
    let b = (rgb >>  0) & 0xFF;
    let supports_LCD = r != g || r != b;
    supports_LCD
}



pub trait ColorSpaceLuminance {
	fn to_luma(&self, gamma: f32, luminance: f32) -> f32;
	fn from_luma(&self, gamma: f32, luma: f32) -> f32;
}

pub struct SRGBColorSpaceLuminance
{
}

pub struct LinearColorSpaceLuminance
{
}

pub struct GammaColorSpaceLuminance
{}

impl ColorSpaceLuminance for LinearColorSpaceLuminance {
	fn to_luma(&self, gamma: f32, luminance: f32) -> f32 {
        assert!(gamma == 1.);
		luminance
	}
	fn from_luma(&self, gamma: f32, luma: f32) -> f32 {
        assert!(gamma == 1.);
		luma
	}
}

impl ColorSpaceLuminance for GammaColorSpaceLuminance {
	fn to_luma(&self, gamma: f32, luminance: f32) -> f32 {
		luminance.powf(gamma)
	}
	fn from_luma(&self, gamma: f32, luma: f32) -> f32 {
		luma.powf(1./gamma)
	}
}

impl ColorSpaceLuminance for SRGBColorSpaceLuminance {
	fn to_luma(&self, gamma: f32, luminance: f32) -> f32 {
        assert!(gamma == 0.);
        //The magic numbers are derived from the sRGB specification.
        //See http://www.color.org/chardata/rgb/srgb.xalter .
		if luminance <= 0.04045 {
			return luminance / 12.92;
		}
		return ((luminance + 0.055) / 1.055).powf(2.4);
	}
	fn from_luma(&self, gamma: f32, luma: f32) -> f32 {
        assert!(gamma == 0.);
        //The magic numbers are derived from the sRGB specification.
        //See http://www.color.org/chardata/rgb/srgb.xalter .
        if luma <= 0.0031308 {
            return luma * 12.92;
        }
        return 1.055 * luma.powf(1./2.4)
               - 0.055;
	}
}

fn round_to_u8(x : f32) -> u8
{
    assert!((x + 0.5).floor() < 256.0);
    (x + 0.5).floor() as u8
}


/**
 * A value of 0.5 for SK_GAMMA_CONTRAST appears to be a good compromise.
 * With lower values small text appears washed out (though correctly so).
 * With higher values lcd fringing is worse and the smoothing effect of
 * partial coverage is diminished.
 */
//rec->setContrast(0.5f);

fn apply_contrast(srca: f32, contrast: f32) -> f32 {
    srca + ((1.0 - srca) * contrast * srca)
}

// The approach here is not necessarily the one with the lowest error
// See https://bel.fi/alankila/lcd/alpcor.html for a similar kind of thing
// that just search for the adjusted alpha value
pub fn build_gamma_correcting_lut(table: &mut [u8; 256], src: u8, contrast: f32,
                                       src_space: &ColorSpaceLuminance, src_gamma: f32,
                                       dst_convert: &ColorSpaceLuminance, dst_gamma: f32) {

    let src = src as f32 / 255.0;
    let lin_src = src_space.to_luma(src_gamma, src);
    //Guess at the dst. The perceptual inverse provides smaller visual
    //discontinuities when slight changes to desaturated colors cause a channel
    //to map to a different correcting lut with neighboring srcI.
    //See https://code.google.com/p/chromium/issues/detail?id=141425#c59 .
    let dst = 1.0 - src;
    let lin_dst = dst_convert.to_luma(dst_gamma, dst);

    //Contrast value tapers off to 0 as the src luminance becomes white
    let adjusted_contrast = contrast * lin_dst;

    //Remove discontinuity and instability when src is close to dst.
    //The value 1/256 is arbitrary and appears to contain the instability.
    if (src - dst).abs() < (1.0 / 256.0) {
        let mut ii : f32 = 0.0;
        for i in 0..256 {
            let raw_srca = ii / 255.0;
            let srca = apply_contrast(raw_srca, adjusted_contrast);

            table[i] = round_to_u8(255.0 * srca);
            ii += 1.0;
        }
    } else {
        // Avoid slow int to float conversion.
        let mut ii : f32 = 0.0;
        for i in 0..256 {
            // 'raw_srca += 1.0f / 255.0f' and even
            // 'raw_srca = i * (1.0f / 255.0f)' can add up to more than 1.0f.
            // When this happens the table[255] == 0x0 instead of 0xff.
            // See http://code.google.com/p/chromium/issues/detail?id=146466
            let raw_srca = ii / 255.0;
            let srca = apply_contrast(raw_srca, adjusted_contrast);
            assert!(srca <= 1.0);
            let dsta = 1.0 - srca;

            // Calculate the output we want.
            let lin_out = lin_src * srca + dsta * lin_dst;
            assert!(lin_out <= 1.0);
            let out = dst_convert.from_luma(dst_gamma, lin_out);

            // Undo what the blit blend will do.
            // i.e. given the formula for OVER: out = src * result + (1 - result) * dst
            // solving for result gives:
            let result = (out - dst) / (src - dst);

            table[i] = round_to_u8(255.0 * result);

            ii += 1.0;
        }
    }
}

use std::cmp;

fn over(dst: u32, src: u32, alpha: u32) -> u32 {
    (src * alpha + dst * (255 - alpha))/255
}

fn overf(dst: f32, src: f32, alpha: f32) -> f32 {
    ((src * alpha + dst * (255. - alpha))/255.) as f32
}


fn absdiff(a: u32, b: u32) -> u32 {
    if a < b  { b - a } else { a - b }
}

#[test]
fn gamma() {
    let mut table: [u8; 256] = unsafe{ mem::uninitialized() } ;
    let space = GammaColorSpaceLuminance{};
    let g : f32 = 2.;
    let mut src : u32 = 131;
    while src < 256 {
        build_gamma_correcting_lut(&mut table, src as u8, 0., &space, g, &space, g);
        let mut max_diff = 0;
        let mut dst = 0;
        while dst < 256 {
            for alpha in 0u32..256 {
                let preblend = table[alpha as usize];
                let lin_dst = (dst as f32 / 255.).powf(g) * 255.;
                let lin_src = (src as f32 / 255.).powf(g) * 255.;

                let preblend_result = over(dst, src, preblend as u32);
                let true_result = ((overf(lin_dst, lin_src, alpha as f32) / 255.).powf(1. / g) * 255.) as u32;
                let diff = absdiff(preblend_result, true_result);
                //println!("{} -- {} {} = {}", alpha, preblend_result, true_result, diff);
                max_diff = cmp::max(max_diff, diff);
            }

            //println!("{} {} max {}", src, dst, max_diff);
            assert!(max_diff <= 33);
            dst += 1;

        }
        src += 1;
    }

}

