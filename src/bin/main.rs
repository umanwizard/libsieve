use bmail::parse::email::message;
use bmail::SmtpEnvelope;
use libsieve::sema::analyze;
use libsieve::{exec::execute, parse::document};
fn main() {
    let script = r#"
    if header :is ["Subject"] [" asdf"] {
      redirect "brennan.vincent@gmail.com";
    }"#;
    let mail = r"#X-Fes-Received-For: brennan@umanwizard.com
X-Fes-Received-From: brennan@umanwizard.com
Received: From: [192.168.1.213] By umanwizard.com ; 19 Sep 2020 16:39:37+0000
List-Id: blah.freebsd.org
From: Brennan Vincent <brennan@umanwizard.com>
Subject: asdf
Message-ID: <979f6797-9f24-1a10-fc34-8f1915f4af56@umanwizard.com>
Date: Sat, 19 Sep 2020 12:39:35 -0400
User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:68.0) Gecko/20100101
 Thunderbird/68.10.0
MIME-Version: 1.0
To: undisclosed-recipients: ;
Content-Type: text/plain; charset=utf-8; format=flowed
Content-Transfer-Encoding: 7bit
Content-Language: en-US
X-Fes-Encrypted:true
X-Fes-Ehlo-Domain:[192.168.1.213]

asdf#"
        .replace('\n', "\r\n");
    let doc = document(&script);
    let ast = analyze(doc.unwrap().1).unwrap();
    println!("{:#?}", ast);
    let (_, mail) = nom::combinator::complete(message)(mail.as_bytes()).unwrap();
    println!("{:?}", mail);
    let env = SmtpEnvelope {
        from: None,
        to: None,
    };
    let actions = execute(&ast, &mail, &env).unwrap();
    println!("{:#?}", actions);
}
