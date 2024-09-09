use super::iasm::{self, IASMProg};

pub fn resolve_labels(prog: IASMProg) -> IASMProg {
    let mut new_prog = IASMProg::default();
    let mut label_map = std::collections::HashMap::new();

    let mut line_no = 0;
    for line in prog.text.iter() {
        match line {
            iasm::IASMLine::Label(label) => {
                label_map.insert(label, line_no);
            }
            _ => line_no += 1,
        }
    }

    for line in prog.text.iter() {
        match line {
            iasm::IASMLine::Statement(statement) => {
                let mut new_statement = statement.clone();
                for param in new_statement.params.iter_mut() {
                    match param {
                        iasm::IASMParam::Label(label) => {
                            let loc = label_map
                                .get(label)
                                .expect(&format!("Expected label {:?} to have a loc", label));
                            label.resolve(*loc);
                        }
                        _ => {}
                    }
                }

                new_prog
                    .text
                    .push_back(iasm::IASMLine::Statement(new_statement));
            }
            iasm::IASMLine::Label(_) => {}
            iasm::IASMLine::Comment(_) => {
                new_prog.text.push_back(line.clone());
            }
        }
    }
    new_prog
}
