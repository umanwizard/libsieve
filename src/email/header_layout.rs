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
        if priority > self.distinct_priorities {
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
            // we can fit everything on the same line. The score is a slice of all zeroes,
            // so we don't need to set it.
            // optim[n] is similarly left at `None`.
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
                let score = self.score(try_break_idx);
                if score < &best_score {
                    for i in 0..best_score.len() {
                        best_score[i] = score[i];
                    }
                    best_score_break_idx = try_break_idx;
                }
            }

            let result_score = self.score_mut(self.breaks.len());
            for i in 0..best_score.len() {
                result_score[i] = best_score[i];
            }

            self.optim[self.breaks.len()] = Some(best_score_break_idx);
        }
        self.breaks.push(Break {
            index: self.buffer.len(),
            priority,
            space,
        });
        Ok(())
    }

    fn score(&self, n: usize) -> &[usize] {
        let l = n * self.distinct_priorities;
        let r = (n + 1) * self.distinct_priorities;
        &self.scores[l..r]
    }

    fn score_mut(&mut self, n: usize) -> &mut [usize] {
        let l = n * self.distinct_priorities;
        let r = (n + 1) * self.distinct_priorities;
        &mut self.scores[l..r]
    }

    // maximum space available in a line
    fn avail(&self, is_first: bool) -> usize {
        self.max_width
            - if is_first {
                0
            } else {
                1 // to accommodate the space for folding
            }
    }
}
