<img alt="rune logo" src="https://raw.githubusercontent.com/rune-rs/rune/main/assets/icon.png" />
<br>
{{#each badges}}
{{literal this.html}}
{{/each}}
<br>
{{#if crate.rust_version}}
Minimum support: Rust <b>{{crate.rust_version}}+</b>.
<br>
{{/if}}
<br>
<a href="https://rune-rs.github.io"><b>Visit the site 🌐</b></a>
&mdash;
<a href="https://rune-rs.github.io/book/"><b>Read the book 📖</b></a>
<br>
<br>

{{crate.description}}
{{#if body}}

<br>

{{literal header_marker~}}
{{literal body}}
{{/if}}
