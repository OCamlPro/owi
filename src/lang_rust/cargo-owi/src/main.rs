use std::os::unix::process::CommandExt;

use clap::{CommandFactory, FromArgMatches, Parser};
use escargot::CargoBuild;

#[derive(Debug, Parser)]
#[command(styles = clap_cargo::style::CLAP_STYLING)]
struct Cli {
    #[command(flatten)]
    manifest: clap_cargo::Manifest,
    #[command(flatten)]
    features: clap_cargo::Features,
    #[arg(short, long, value_name = "SPEC")]
    /// Package to process (see `cargo help pkgid`)
    package: Option<String>,
    #[command(subcommand)]
    subcommand: Subcmd,
}

#[derive(Debug, Parser)]
#[command(styles = clap_cargo::style::CLAP_STYLING)]
enum Subcmd {
    Sym,
}

fn main() -> anyhow::Result<()> {
    let mut acc = 0;
    let args = std::env::args_os()
        .filter(|s| {
            let is_second = {
                let res = acc == 1;
                acc += 1;
                res
            };
            let is_owi = s == "owi";
            !(is_second && is_owi)
        })
        .take_while(|s| s != "--");

    let mut command = Cli::command();
    let mut generated_usage = command.render_usage();
    generated_usage.push_str("\u{1b}[36m [-- [OWI_OPTIONS]]\u{1b}[0m");
    command = command.override_usage(generated_usage);
    let mut matches = command.get_matches_from(args);
    let res = Cli::from_arg_matches_mut(&mut matches);
    let cli = match res {
        Ok(s) => s,
        Err(e) => e.exit(),
    };

    match cli.subcommand {
        Subcmd::Sym => cmd_sym(cli),
    }
}

fn cmd_sym(cli: Cli) -> anyhow::Result<()> {
    let features = cli.features.features.join(" ");
    let mut build_command = CargoBuild::new()
        .release()
        .features(features)
        .target("wasm32-unknown-unknown ");
    if let Some(p) = cli.manifest.manifest_path {
        build_command = build_command.manifest_path(p);
    }
    if let Some(pkg) = cli.package {
        build_command = build_command.package(pkg)
    }
    let resulting_binary = build_command.run()?;
    let mut owi_command = std::process::Command::new("owi");
    let pass_through_args = std::env::args_os().skip_while(|s| s != "--").skip(1);
    owi_command
        .args(["sym".as_ref(), resulting_binary.path().as_os_str()])
        .args(["--entry-point=main", "--unsafe"])
        .args(pass_through_args);
    let diverge_or_err = owi_command.exec();
    Err(diverge_or_err.into())
}
