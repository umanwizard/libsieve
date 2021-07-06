#[derive(Copy, Clone, Debug)]
struct Break {
    index: usize,
    priority: usize,
    space: bool,
}
pub struct HeaderFieldFormatter {
    max_width: usize,
    distinct_priorities: usize,
    // scores[n * distinct_priorities + k]
    // is the number of breaks of priority k
    // in the optimal solution for tokens
    // 0..=n.
    scores: Vec<usize>,
    // optim[n] is the index of the
    // last break (if any) in the optimal solution for
    // tokens 0..=n.
    optim: Vec<Option<usize>>,
    buffer: Vec<u8>,
    // potential breaks
    breaks: Vec<Break>,
}

#[derive(Debug, Clone, Copy)]
pub struct FormatError;

impl HeaderFieldFormatter {
    pub fn new(
        max_width: usize,
        distinct_priorities: usize,
        initial_text: &[u8],
        initial_break_priority: usize,
        initial_text_space: bool,
    ) -> Self {
        let buffer = if initial_text_space {
            let mut buffer = Vec::with_capacity(initial_text.len() + 1);
            buffer.extend_from_slice(initial_text);
            buffer.push(b' ');
            buffer
        } else {
            initial_text.to_vec()
        };
        Self {
            max_width,
            distinct_priorities,
            scores: vec![0; distinct_priorities],
            optim: vec![None],
            buffer,
            breaks: vec![Break {
                index: initial_text.len() + (initial_text_space as usize),
                priority: initial_break_priority,
                space: initial_text_space,
            }],
        }
    }
    pub fn push(&mut self, token: &[u8], priority: usize, space: bool) -> Result<(), FormatError> {
        if priority >= self.distinct_priorities {
            panic!()
        }
        if token.len() > self.avail(false) {
            return Err(FormatError);
        }
        self.buffer.extend(token);
        let pre_space_len = self.buffer.len();
        if space {
            self.buffer.push(b' ');
        }
        if pre_space_len <= self.avail(true) {
            // We can fit everything on the same line
            // Make sure there are zeros...
            self.next_score(self.breaks.len());
            self.optim.push(None);
        } else {
            // We have to break somewhere.
            // Try all the possible breakpoints starting at the end.
            // (I.e.: try one token on the last line, then two, then three, and so on,
            //  until we either reach the beginning of the tokens or we run out of space on the line.)
            //
            // The score for each scenario is the optimum score up to the broken token,
            // plus one in the coordinate of the priority of that token's break.
            assert!(
                !self.breaks.is_empty(),
                "Breaking after the field name is always possible"
            );
            // We know we can fit at least one token on this line
            // (because otherwise we would have already returned
            //  an error), so initialize "best score"
            // to breaking after the former last token
            let mut try_break_idx = self.breaks.len() - 1;
            let mut best_score = self.score(try_break_idx).to_vec();
            best_score[self.breaks[try_break_idx].priority] += 1;
            let mut best_score_break_idx = try_break_idx;

            while try_break_idx > 0 {
                try_break_idx -= 1;
                let r#break = self.breaks[try_break_idx];
                let potential_line_width = pre_space_len - r#break.index;
                if potential_line_width > self.avail(false) {
                    break;
                }
                let mut score = self.score(try_break_idx).to_vec();
                score[self.breaks[try_break_idx].priority] += 1;
                if &score < &best_score {
                    for i in 0..best_score.len() {
                        best_score[i] = score[i];
                    }
                    best_score_break_idx = try_break_idx;
                }
            }

            let result_score = self.next_score(self.breaks.len());
            for i in 0..best_score.len() {
                result_score[i] = best_score[i];
            }

            self.optim.push(Some(best_score_break_idx));
        }
        self.breaks.push(Break {
            index: self.buffer.len(),
            priority,
            space,
        });
        Ok(())
    }

    pub fn done(self, ret: &mut Vec<u8>) {
        let mut cur_break_and_idx = match self.optim.last().unwrap() {
            Some(break_idx) => Some((self.breaks[*break_idx], *break_idx)),
            None => {
                if self.breaks.last().unwrap().space {
                    ret.extend_from_slice(&self.buffer[0..self.buffer.len() - 1]);
                    ret.push(b'\r');
                    ret.push(b'\n');
                } else {
                    ret.extend_from_slice(&self.buffer);
                    ret.push(b'\r');
                    ret.push(b'\n');
                };
                return;
            }
        };
        let mut indices = vec![];
        let mut to_process_r = self.buffer.len() - self.breaks.last().unwrap().space as usize;
        while let Some((cur_break, cur_break_idx)) = cur_break_and_idx {
            let r = to_process_r;
            let l = cur_break.index;
            indices.push((l, r));
            to_process_r = l - cur_break.space as usize;
            cur_break_and_idx = self.optim[cur_break_idx].map(|idx| (self.breaks[idx], idx));
        }
        indices.push((0, to_process_r));
        let mut first = true;
        for (l, r) in indices.iter().rev().copied() {
            if !first {
                ret.push(b' ');
            }
            first = false;
            ret.extend_from_slice(&self.buffer[l..r]);
            ret.push(b'\r');
            ret.push(b'\n');
        }
    }

    fn score(&self, n: usize) -> &[usize] {
        let l = n * self.distinct_priorities;
        let r = (n + 1) * self.distinct_priorities;
        &self.scores[l..r]
    }

    fn next_score(&mut self, n: usize) -> &mut [usize] {
        assert!(n * self.distinct_priorities == self.scores.len());
        self.scores.resize((n + 1) * self.distinct_priorities, 0);
        let l = n * self.distinct_priorities;
        let r = (n + 1) * self.distinct_priorities;
        &mut self.scores[l..r]
    }

    // maximum space available in a line
    fn avail(&self, is_first: bool) -> usize {
        self.max_width - ((!is_first) as usize) // to accommodate the space for folding
    }
}

// Test script; use as follows:
// 0!brennan
// 0!@
// 0!umanwizard.com
// 1 ;
// 0!mary-poppins-supercalifragilisticexpialidocious
// 0!@
// 0!mary-poppins-supercalifragilisticexpialidocious.com
// 1 ;
// 0!some-normal-address
// 0!@
// 0!hotmail.com
// 1 ;
// 0!brennan.vincent
// 0!@
// 0!gmail.com

fn main() {
    use std::io::stdin;
    use std::io::BufRead;

    let mut hff = HeaderFieldFormatter::new(78, 3, b"To:", 2, true);
    for line in stdin().lock().lines() {
        let line = line.unwrap();
        let chs = line.as_bytes();
        let mut prio = 0;
        let mut cursor = 0;
        while chs[cursor].is_ascii_digit() {
            prio *= 10;
            prio += (chs[cursor] - b'0') as usize;
            cursor += 1;
        }

        let space = match chs[cursor] {
            b' ' => true,
            b'!' => false,
            _ => panic!("{}", line),
        };

        hff.push(&chs[cursor + 1..], prio, space).unwrap();
    }
    let mut result = vec![];
    hff.done(&mut result);
    print!("{}", std::str::from_utf8(&result).unwrap());
}
