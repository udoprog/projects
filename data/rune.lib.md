<img alt="rune logo" src="https://raw.githubusercontent.com/rune-rs/rune/main/assets/icon.png" />
<br>
<a href="https://rune-rs.github.io"><b>Visit the site 🌐</b></a>
&mdash;
<a href="https://rune-rs.github.io/book/"><b>Read the book 📖</b></a>
<br>
{{#each badges}}
{{literal this.html}}
{{/each}}

<br>
<br>

{{crate.description}}
{{#if rest}}

<br>

{{literal header_marker~}}
{{literal rest}}
{{/if}}
