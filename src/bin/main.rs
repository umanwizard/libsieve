use libsieve_rs::parse::document;
use libsieve_rs::sema::analyze;
fn main() {
    let hmm = "\r\n #\r\n # Example Sieve Filter\r\n # Declare any optional features or extension used by the script\r\n #\r\n require [\"fileinto\"];\r\n \r\n #\r\n # Handle messages from known mailing lists\r\n # Move messages from IETF filter discussion list to filter mailbox\r\n #\r\n if header :is \"Sender\" \"owner-ietf-mta-filters@imc.org\"\r\n {\r\n fileinto \"filter\";  # move to \"filter\" mailbox\r\n }\r\n #\r\n # Keep all messages to or from people in my company\r\n #\r\n elsif address :DOMAIN :is [\"From\", \"To\"] \"example.com\"\r\n {\r\n keep;               # keep in \"In\" mailbox\r\n }\r\n \r\n #\r\n # Try and catch unsolicited email.  If a message is not to me,\r\n # or it contains a subject known to be spam, file it away.\r\n #\r\n elsif anyof (NOT address :all :contains\r\n [\"To\", \"Cc\", \"Bcc\"] \"me@example.com\",\r\n header :matches \"subject\"\r\n [\"*make*money*fast*\", \"*university*dipl*mas*\"])\r\n {\r\n fileinto \"spam\";   # move to \"spam\" mailbox\r\n }\r\n else\r\n {\r\n # Move all other (non-company) mail to \"personal\"\r\n # mailbox.\r\n fileinto \"personal\";\r\n }   ";
    let doc = document(hmm);
    println!("{:#?}", analyze(doc));
}
