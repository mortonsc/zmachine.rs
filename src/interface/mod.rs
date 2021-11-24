use log::info;

pub trait Interface {
    fn erase_window(&mut self, window: usize);
    fn set_fg_color(&mut self, color: usize);
    fn get_fg_color(&self) -> usize;
    fn set_bg_color(&mut self, color: usize);
    fn get_bg_color(&self) -> usize;
    fn set_text_style(&mut self, style: usize);
    fn get_text_style(&self) -> usize;

    fn print(&mut self, text: &str);
    fn println(&mut self, text: &str);
}

pub struct DebugInterface {
    fg_color: usize,
    bg_color: usize,
    text_style: usize,
}

impl DebugInterface {
    pub fn new() -> Self {
        DebugInterface {
            fg_color: 1,
            bg_color: 1,
            text_style: 0,
        }
    }
}

impl Interface for DebugInterface {
    fn erase_window(&mut self, window: usize) {
        info!("erase window {}", window);
    }

    fn set_fg_color(&mut self, color: usize) {
        info!("set fg color to {}", color);
        self.fg_color = color;
    }

    fn get_fg_color(&self) -> usize {
        self.fg_color
    }

    fn set_bg_color(&mut self, color: usize) {
        info!("set bg color to {}", color);
        self.bg_color = color;
    }

    fn get_bg_color(&self) -> usize {
        self.bg_color
    }

    fn set_text_style(&mut self, style: usize) {
        info!("set text style to {}", style);
        self.text_style = style;
    }

    fn get_text_style(&self) -> usize {
        self.text_style
    }

    fn print(&mut self, text: &str) {
        print!("{}", text);
    }

    fn println(&mut self, text: &str) {
        println!("{}", text);
    }
}
