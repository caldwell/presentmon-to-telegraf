PresentMon-to-Telegraf
----------------------

Runs [PresentMon][1] and massages the output into a format suitable for
[InfluxDB's input line format][2]. This lets me get the current running game
exe name, the current FPS, and some frame timing statistics into InfluxDB
via Telegraf.

[1]: https://github.com/GameTechDev/PresentMon
[2]: https://docs.influxdata.com/influxdb/v1.7/write_protocols/line_protocol_tutorial/

I hook into Telegraf thusly:

    [[inputs.exec]]
      commands = [
         '''"C:\Program Files\Telegraf\presentmon-to-telegraf.exe" --exe "C:\Program Files\Telegraf\PresentMon-1.8.0-x64.exe"'''
      ]
      timeout = "5s"
      data_format = "influx"

## Building

    cargo build

### Cross-compiling

    cargo build --target x86_64-pc-windows-gnu

Copyright and License
---------------------

License: [ISC](https://en.wikipedia.org/wiki/ISC_license)

Copyright © 2023 David Caldwell \<david@porkrind.org\>

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
