use docopt::Docopt;
use serde::{Deserialize,Deserializer};

use std::collections::HashMap;
use std::error::Error;
use std::process::Command;
use std::process::Stdio;
use std::sync::Once;

#[derive(Debug, Deserialize, Clone)]
#[allow(dead_code)]
struct Frame {
    #[serde(rename = "Application")]            application:                String, // WindowsTerminal.exe
    #[serde(rename = "ProcessID")]              process_id:                 u32,    // 17076
    #[serde(rename = "SwapChainAddress")]       swap_chain_address:         u64,    // 0x00000234ED34B070
    #[serde(rename = "Runtime")]                runtime:                    String, // DXGI
    #[serde(rename = "SyncInterval")]           sync_interval:              u32,    // 1
    #[serde(rename = "PresentFlags")]           present_flags:              u32,    // 0
    #[serde(rename = "Dropped")]                dropped:                    u32,    // 0
    #[serde(rename = "TimeInSeconds")]          time_in_seconds:            f64,    // 0.10710200000000
    #[serde(rename = "msInPresentAPI")]         ms_in_present_api:          f64,    // 0.04180000000000
    #[serde(rename = "msBetweenPresents")]      ms_between_presents:        f64,    // 93.46039999999999
    #[serde(deserialize_with = "bool_from_int")]
    #[serde(rename = "AllowsTearing")]          allows_tearing:             bool,   // 0
    #[serde(rename = "PresentMode")]            present_mode:               String, // Composed: Flip
    #[serde(rename = "msUntilRenderComplete")]  ms_until_render_complete:   f64,    // 1.38210000000000
    #[serde(rename = "msUntilDisplayed")]       ms_until_displayed:         f64,    // 34.20570000000000
    #[serde(rename = "msBetweenDisplayChange")] ms_between_display_change:  f64,    // 116.71270000000000
}

fn bool_from_int<'de, D>(deserializer: D) -> Result<bool, D::Error>
where
    D: Deserializer<'de>,
{
    match u8::deserialize(deserializer)? {
        0 => Ok(false),
        1 => Ok(true),
        other => Err(serde::de::Error::invalid_value(serde::de::Unexpected::Unsigned(other as u64), &"0 or 1")),
    }
}

const USAGE: &'static str = "
Usage:
  presentmon-to-telegraf [-h] [-v...] [-e <PATH>] [--hostname <HOSTNAME>]

Options:
  -h --help              Show this screen.
  -v --verbose           Dial up the verbosity.
  -e --exe=<PATH>        PresentMon exe path [default: \"PresentMon-1.8.0-x64.exe\"]
";

#[derive(Debug, Deserialize)]
struct Args {
    flag_verbose:    usize,
    flag_exe:        String,
}

static mut VERBOSE: usize = 0;
static VERBOSE_INIT: Once = Once::new();
fn verbose(level: usize) -> bool { unsafe { VERBOSE >= level } } // A lot of effort for a global in a single threaded app. :-/

fn main() -> Result<(), Box<dyn Error>> {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());

    unsafe { VERBOSE_INIT.call_once(|| { VERBOSE = args.flag_verbose; }) };

    let frames = get_frames(&args.flag_exe)?;

    if verbose(1) { println!("{:?}", frames) }

    let (app_name, frames) = dominant_app(frames)?;

    if verbose(1) { println!("App={}, frames={}", app_name, frames.len()) }

    let stats = compute_stats(&frames);

    let tags = HashMap::from([
        ("host".to_string(), hostname()?)
    ]);
    print!("{}", influxdb_format(app_name_changed(app_name)?, stats, tags));

    Ok(())
}

fn capture(cmd: &str, args: Vec<&str>) -> Result<String, Box<dyn Error>> {
    if verbose(1) { println!("+ {cmd} {args:?}") }
    let child = Command::new(cmd)
        .args(args)
        .stdout(Stdio::piped())
        .spawn()
        .map_err(|e| format!("couldn't run '{cmd}': {e}"))?;
    let out = child
        .wait_with_output()
        .map_err(|e| format!("failed to wait on child for {cmd}: {e}"))?
        .stdout;
    Ok(std::str::from_utf8(&out)
       .map_err(|e| format!("bad utf8 in output of {cmd}: {e} in {:?}", &out))?.to_owned())
}

fn hostname() -> Result<String, Box<dyn Error>> {
    Ok(capture("hostname", vec![])?.trim().to_string())
}

fn get_frames(exe: &str) -> Result<Vec<Frame>, Box<dyn Error>> {
    let rawcsv = capture(exe, vec!["--timed", "2",
                                   "--terminate_after_timed",
                                   "--output_stdout"])?;
    if verbose(1) { println!("{rawcsv}") };

    Ok(csv::Reader::from_reader(rawcsv.as_bytes())
       .deserialize()
       .collect::<Result<_, csv::Error>>()
       .map_err(|e| format!("failed to parse csv:\n{rawcsv}\n {e}"))?)
}

fn dominant_app(frames: Vec<Frame>) -> Result<(String, Vec<Frame>), Box<dyn Error>> {
    let mut app: HashMap<String,Vec<Frame>> = HashMap::new();

    let mut dominant = None;

    for frame in frames {
        if frame.application == "dwm.exe" { continue } // Ignore Windows "Desktop Window Manager".
        app.entry(frame.application.clone()).and_modify(|f| f.push(frame.clone())).or_insert(vec![frame.clone()]);
        dominant = match (&dominant, frame.application) {
            (None, a)                                                     => Some(a),
            (Some(dom), a) if app.get(&a).unwrap().len() > app[dom].len() => Some(a),
            (_,_)                                                         => dominant,
        };
    }
    Ok((dominant.as_ref().ok_or("No frames?")?.to_string(), app.remove(dominant.as_ref().unwrap()).unwrap()))
}


struct Stats {
    fps: usize,
    fps_ave: f64,
    fps_p99: f64,
    fps_p95: f64,
    frame_time_ms_ave: f64,
    frame_time_ms_stddev: f64,
    frame_time_ms_p99: f64,
    frame_time_ms_p95: f64,
}

fn mean<I,T>(vals: I) -> T
where I: Iterator<Item = T> + ExactSizeIterator,
      T: std::ops::Add<Output = T> + std::ops::Div<Output = T> + std::convert::From<u32> {
    let (sum, count) = vals.fold((T::from(0),T::from(0)), |acc, v| (acc.0 + v, acc.1 + T::from(1)));
    sum / count
}

fn percentile<I,T>(p: f64, vals: I) -> T
where I: Iterator<Item = T> + ExactSizeIterator,
      T: std::ops::Add<Output = T> + std::ops::Div<Output = T> + std::ops::Sub<Output = T> + std::ops::Mul<Output = T> +  std::cmp::PartialOrd + std::convert::From<f64> + Copy + core::fmt::Debug {
    let mut v: Vec<T> = vals.collect();
    v.sort_unstable_by(|a,b| a.partial_cmp(b).unwrap()/*I'm not thinking about NaNs*/);
    if verbose(2) { for (i, _v) in v.iter().enumerate() { println!("{i}: {_v:?}") } }
    // Inclusive Interpolated (since it can to 95th and 99th percentil with lower counts): https://en.wikipedia.org/wiki/Percentile
    // let or = p / 100.0 * (v.len() + 1) as f64 - 1.0; // Exclusive
    let or = p / 100.0 * (v.len() - 1) as f64; // Inclusive
    let lo_score: T = v[or as usize];
    let hi_score: T = v[or.ceil() as usize];
    let diff: T = hi_score - lo_score;
    lo_score + diff * T::from(or.fract())
}

fn compute_stats(all_frames: &Vec<Frame>) -> Stats {
    // We grab 2 seconds worth of frames to make sure we have a full seconds worth (we seem to get less for some reason)
    let frames = all_frames.iter().filter(|f| f.time_in_seconds < all_frames[0].time_in_seconds + 1.0).collect::<Vec<_>>();
    if verbose(2) {
        println!("frames = {}", frames.len());
        println!("fps = (frames.len()[{}] as f64 / (frames.last().unwrap().time_in_seconds[{}] - frames.first().unwrap().time_in_seconds[{}]))[{}] as usize -> [{}]",
                 frames.len(), frames.last().unwrap().time_in_seconds, frames.first().unwrap().time_in_seconds,
                 frames.last().unwrap().time_in_seconds - frames.first().unwrap().time_in_seconds,
                 (frames.len() as f64 / (frames.last().unwrap().time_in_seconds - frames.first().unwrap().time_in_seconds)) as usize);
    }
    let frame_time_ms_ave = mean(frames.iter().map(|f| f.ms_between_display_change));
    let frame_time_ms_p99 = percentile(99.0, frames.iter().map(|f| f.ms_between_display_change));
    let frame_time_ms_p95 = percentile(95.0, frames.iter().map(|f| f.ms_between_display_change));
    Stats {
        fps: (frames.len() as f64 / (frames.last().unwrap().time_in_seconds - frames.first().unwrap().time_in_seconds)) as usize,
        fps_ave: 1000.0 / frame_time_ms_ave,
        fps_p99: 1000.0 / frame_time_ms_p99,
        fps_p95: 1000.0 / frame_time_ms_p95,
        frame_time_ms_stddev: mean(frames.iter().map(|f| { let d = frame_time_ms_ave - f.ms_between_presents; d*d })).sqrt(),
        frame_time_ms_ave,
        frame_time_ms_p99,
        frame_time_ms_p95,
    }
}

fn app_name_changed(app_name: String) -> Result<Option<String>, Box<dyn Error>> {
    let state_file = dirs::data_local_dir().unwrap_or(std::path::Path::new("/").into()).join("PresentMon-to-telegraf.last_appname.txt");
    let last_app_name = String::from_utf8(std::fs::read(&state_file).unwrap_or(vec![]))?;
    if last_app_name == app_name {
        return Ok(None);
    }
    std::fs::write(state_file, app_name.as_bytes())?;
    Ok(Some(app_name))
}

fn influxdb_format(app_name: Option<String>, stats: Stats, tags: HashMap<String,String>) -> String {
    let tags_str = tags.iter().map(|(k,v)| format!(",{k}={v}")).collect::<String>();

    (match app_name {
        Some(name) => format!("active_app{tags_str} name=\"{name}\"\n"),
        _ => "".to_string(),
    })
        + &format!("framerate{tags_str} fps={},fps_ave={},fps_p99={},fps_p95={},frame_time_ms_ave={},frame_time_ms_stddev={},frame_time_ms_p99={},frame_time_ms_p95={}",
                   stats.fps, stats.fps_ave, stats.fps_p99, stats.fps_p95, stats.frame_time_ms_ave, stats.frame_time_ms_stddev, stats.frame_time_ms_p99, stats.frame_time_ms_p95)
}
