use wasm_bindgen::prelude::*;
use int_rs::eval::eval;

#[wasm_bindgen]
pub async fn run(input: &str) -> JsValue {
    match eval(input) {
        Ok(s) => s.into(),
        Err(e) => format!("{:?}", e).into(),
    }
}
