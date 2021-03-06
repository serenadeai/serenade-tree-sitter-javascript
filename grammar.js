module.exports = grammar({
  name: 'javascript',

  externals: $ => [$._automatic_semicolon, $._template_chars],

  extras: $ => [$.comment, /[\s\uFEFF\u2060\u200B\u00A0]/],

  supertypes: $ => [],

  inline: $ => [
    $._call_signature,
    $._expressions,
    $._semicolon,
    $._identifier,
    $._jsx_attribute,
    $._jsx_element_name,
    $._jsx_element,
    $._jsx_attribute_name,
    $._jsx_attribute_value,
    $._lhs_expression,
  ],

  precedences: $ => [
    [
      'member',
      'call',
      $.update_expression,
      'unary_not',
      'unary_void',
      'binary_exp',
      'binary_times',
      'binary_plus',
      'binary_compare',
      'binary_relation',
      'binary_in',
      'binary_and',
      'binary_or',
      'ternary',
      $.await_expression,
      $.sequence_expression,
      $.lambda,
    ],
    [$.rest_pattern, 'assign'],
    ['assign', $.primary_expression],
    ['member', 'new', 'call', $.expression],
    ['declaration', 'literal'],
    [$.primary_expression, $.enclosed_body, 'object'],
    [$.import, $.import_token],
    [$.export_statement, $.primary_expression],
  ],

  conflicts: $ => [
    [$.primary_expression, $.property_name],
    [$.primary_expression, $.property_name, $.lambda],
    [$.primary_expression, $.lambda],
    [$.primary_expression, $.method_definition],
    [$.primary_expression, $.rest_pattern],
    [$.primary_expression, $.pattern],
    [$.primary_expression, $.block_iterator],
    [$.array, $.array_pattern],
    [$.object, $.object_pattern],
    [$.assignment, $.pattern],
    [$.assignment, $.object_assignment_pattern],
    [$.labeled_statement, $.property_name],
    [$.computed_property_name, $.array],

    // New
    [$.computed_property_name, $.array_element],
    [$.if],
    [$.else_if_clause, $.if_clause],

    [$.primary_expression, $.async_modifier],
    [$.primary_expression, $.async_modifier, $.property_name],
    [$.primary_expression, $.static_modifier],
    [$.primary_expression, $.accessors_modifier],

    [$.async_modifier, $.reserved_identifier_],
    [$.export_statement, $.reserved_identifier_],
    [$.static_modifier, $.reserved_identifier_],
    [$.accessors_modifier, $.reserved_identifier_],

    [$.object_assignment_pattern, $.identifier_or_reserved],
    [$.pattern, $.identifier_or_reserved],
    [$.primary_expression, $.identifier_or_reserved],
    [$.rest_pattern, $.identifier_or_reserved],
    [$.property_name, $.identifier_or_reserved],

    [$.object, $.object_pattern_element, $.identifier_or_reserved],
    [$.object, $.object_pattern_element],
  ],

  word: $ => $.identifier,

  rules: {
    program: $ =>
      seq(
        optional($.hash_bang_line),
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    statement_list: $ => repeat1($.statement),

    hash_bang_line: $ => /#!.*/,

    // Modifiers
    async_modifier: $ => field('modifier', 'async'),
    await_modifier: $ => field('modifier', 'await'),
    static_modifier: $ => field('modifier', 'static'),
    accessors_modifier: $ => field('modifier', choice('get', 'set', '*')),

    //
    // Export declarations
    //

    export_statement: $ =>
      choice(
        seq(
          'export',
          choice(
            seq('*', $._from_clause, $._semicolon),
            seq($.namespace_import, $._from_clause, $._semicolon),
            seq($.export_clause, $._from_clause, $._semicolon),
            seq($.export_clause, $._semicolon)
          )
        ),
        seq(
          optional_with_placeholder('decorator_list', repeat($.decorator)),
          'export',
          choice(
            field('declaration', $.declaration),
            seq(
              'default',
              choice(
                field('declaration', $.declaration),
                seq(field('value', $.expression), $._semicolon)
              )
            )
          )
        )
      ),

    export_clause: $ =>
      seq(
        '{',
        commaSep(alias($._import_export_specifier, $.export_specifier)),
        optional(','),
        '}'
      ),

    _import_export_specifier: $ =>
      seq(
        field('name', $.identifier),
        optional(seq('as', field('alias', $.identifier)))
      ),

    declaration: $ =>
      choice(
        alias($.function_declaration, $.function),
        $.generator_function_declaration,
        alias($.class_declaration, $.class),
        field(
          'variable_declaration',
          choice($.lexical_declaration, $.variable_declaration)
        )
      ),

    //
    // Import declarations
    //

    import_token: $ => token('import'),

    import: $ =>
      seq(
        'import',
        choice(seq($.import_clause, $._from_clause), field('source', $.string)),
        $._semicolon
      ),

    import_clause: $ =>
      choice(
        $.namespace_import,
        $.named_imports,
        seq(
          $.identifier,
          optional(seq(',', choice($.namespace_import, $.named_imports)))
        )
      ),

    _from_clause: $ => seq('from', field('source', $.string)),

    namespace_import: $ => seq('*', 'as', $.identifier),

    named_imports: $ =>
      seq(
        '{',
        optional_with_placeholder(
          'import_specifier_list',
          commaSep(alias($._import_export_specifier, $.import_specifier))
        ),
        optional(','),
        '}'
      ),

    //
    // Statements
    //

    statement: $ =>
      choice(
        $.export_statement,
        $.import,
        $.debugger_statement,
        $.expression_statement,
        $.declaration,
        $.enclosed_body,

        $.if,
        $.switch_statement,
        $.for,
        $.while,
        $.do_statement,
        $.try,
        $.with_statement,

        $.break_statement,
        $.continue_statement,
        $.return,
        $.throw_statement,
        $.empty_statement,
        $.labeled_statement
      ),

    expression_statement: $ => seq($._expressions, $._semicolon),

    variable_declaration: $ =>
      seq(
        'var',
        field(
          'assignment_list',
          commaSep1(alias($.variable_declarator, $.assignment))
        ),
        $._semicolon
      ),

    lexical_declaration: $ =>
      seq(
        choice('let', 'const'),
        field(
          'assignment_list',
          commaSep1(alias($.variable_declarator, $.assignment))
        ),
        $._semicolon
      ),

    variable_declarator: $ =>
      seq(
        field(
          'assignment_variable',
          choice($.identifier, $._destructuring_pattern)
        ),
        optional_with_placeholder(
          'assignment_value_list_optional',
          $.assignment_initializer
        )
      ),

    enclosed_body: $ =>
      prec.right(
        seq(
          '{',
          optional_with_placeholder('statement_list', $.statement_list),
          '}',
          optional($._automatic_semicolon)
        )
      ),

    else_clause: $ => seq('else', $.statement),

    else_if_clause: $ =>
      prec.dynamic(
        1,
        seq(
          'else',
          'if',
          '(',
          field('condition', $.parenthesized_expression_inner),
          ')',
          field('consequence', $.statement)
        )
      ),

    // Penalize this so it doesn't get chosen in else if's.
    if_clause: $ =>
      prec.dynamic(
        0,
        seq(
          'if',
          '(',
          field('condition', $.parenthesized_expression_inner),
          ')',
          field('consequence', $.statement)
        )
      ),

    if: $ =>
      seq(
        $.if_clause,
        optional_with_placeholder(
          'else_if_clause_list',
          repeat($.else_if_clause)
        ),
        optional_with_placeholder('else_clause_optional', $.else_clause)
      ),

    switch_statement: $ =>
      seq(
        'switch',
        field('value', $.parenthesized_expression),
        field('body', $.switch_body)
      ),

    for: $ => choice($.for_clause, $.for_each_clause),

    block_initializer: $ =>
      choice(
        field(
          'variable_declaration',
          choice($.lexical_declaration, $.variable_declaration)
        ),
        $.expression_statement
      ),

    for_clause: $ =>
      seq(
        'for',
        '(',
        field(
          'block_initializer_optional',
          choice($.block_initializer, $.empty_statement)
        ),
        field(
          'condition_optional',
          choice(alias($.expression_statement, $.condition), $.empty_statement)
        ),
        optional_with_placeholder(
          'block_update_optional',
          alias($._expressions, $.block_update)
        ),
        ')',
        field('body', $.statement)
      ),

    for_each_clause: $ =>
      seq(
        'for',
        optional_with_placeholder('modifier_list', $.await_modifier),
        '(',
        $.block_iterator,
        field('for_each_separator', choice('in', 'of')),
        field('block_collection', $._expressions),
        ')',
        field('body', $.statement)
      ),

    block_iterator: $ =>
      choice(
        choice($._lhs_expression, $.parenthesized_expression), // left
        seq(
          choice('var', 'let', 'const'),
          field('left', choice($.identifier, $._destructuring_pattern))
        )
      ),

    while: $ => $.while_clause,

    while_clause: $ =>
      seq(
        'while',
        '(',
        field('condition', $.parenthesized_expression_inner),
        ')',
        field('body', $.statement)
      ),

    do_statement: $ =>
      seq(
        'do',
        field('body', $.statement),
        'while',
        field('condition', $.parenthesized_expression),
        $._semicolon
      ),

    try_clause: $ => seq('try', field('body', $.enclosed_body)),

    try: $ =>
      seq(
        $.try_clause,
        optional_with_placeholder('catch_list', $.catch),
        optional_with_placeholder('finally_clause_optional', $.finally_clause)
      ),

    with_statement: $ =>
      seq(
        'with',
        field('object', $.parenthesized_expression),
        field('body', $.statement)
      ),

    break_statement: $ =>
      seq(
        'break',
        optional(alias($.identifier, $.statement_identifier)),
        $._semicolon
      ),

    continue_statement: $ =>
      seq(
        'continue',
        optional(alias($.identifier, $.statement_identifier)),
        $._semicolon
      ),

    debugger_statement: $ => seq('debugger', $._semicolon),

    return: $ =>
      seq(
        'return',
        optional_with_placeholder('return_value_optional', $.return_value),
        $._semicolon
      ),

    return_value: $ => $._expressions,

    throw_statement: $ =>
      seq(field('throw', seq('throw', $._expressions)), $._semicolon),

    empty_statement: $ => ';',

    labeled_statement: $ =>
      prec.dynamic(
        -1,
        seq(
          field('label', choice($.identifier, $.reserved_identifier_)),
          ':',
          $.statement
        )
      ),

    //
    // Statement components
    //

    switch_body: $ => seq('{', repeat(choice($.case, $.switch_default)), '}'),

    case: $ =>
      seq('case', field('value', $._expressions), ':', repeat($.statement)),

    switch_default: $ => seq('default', ':', repeat($.statement)),

    catch: $ =>
      seq(
        'catch',
        optional(seq('(', $.catch_parameter, ')')),
        field('body', $.enclosed_body)
      ),

    catch_parameter: $ => choice($.identifier, $._destructuring_pattern),

    finally_clause: $ => seq('finally', field('body', $.enclosed_body)),

    parenthesized_expression: $ =>
      seq('(', $.parenthesized_expression_inner, ')'),

    // Serenade: We extract this out since parenthesized_expression is overriden in typescript.
    parenthesized_expression_inner: $ => $._expressions,

    //
    // Expressions
    //
    _expressions: $ => choice($.expression, $.sequence_expression),

    expression: $ =>
      choice(
        $.primary_expression,
        $._jsx_element,
        $.jsx_fragment,
        $.assignment,
        $.augmented_assignment_expression,
        $.await_expression,
        $.unary_expression,
        $.binary_expression,
        $.ternary_expression,
        $.update_expression,
        $.new_expression,
        $.yield_expression
      ),

    primary_expression: $ =>
      choice(
        $.subscript_expression,
        $.member_expression,
        $.parenthesized_expression,
        $.identifier_or_reserved,
        $.this,
        $.super,
        $.number,
        $.string,
        $.template_string,
        $.regex,
        $.true,
        $.false,
        $.null,
        $.import_token,
        $.object,
        $.array,
        $.function,
        $.lambda,
        $.generator_function,
        $.class,
        $.meta_property,
        $.call
      ),

    yield_expression: $ =>
      prec.right(
        seq('yield', choice(seq('*', $.expression), optional($.expression)))
      ),

    object: $ =>
      prec(
        'object',
        seq(
          '{',
          optional_with_placeholder(
            'key_value_pair_list',
            commaSep(
              optional(
                choice(
                  $.key_value_pair,
                  $.spread_element,
                  $.method_definition,
                  choice($.identifier, $.reserved_identifier_)
                )
              )
            )
          ),
          '}'
        )
      ),

    object_pattern: $ =>
      prec(
        'object',
        seq(
          '{',
          optional_with_placeholder(
            'object_pattern_list',
            commaSep(
              optional(alias($.object_pattern_element, $.pattern_list_element))
            )
          ),
          '}'
        )
      ),

    object_pattern_element: $ =>
      choice(
        $.pair_pattern,
        $.rest_pattern,
        $.object_assignment_pattern,
        choice($.identifier, $.reserved_identifier_)
      ),

    assignment_pattern: $ =>
      seq(field('left', $.pattern), '=', field('right', $.expression)),

    object_assignment_pattern: $ =>
      seq(
        field(
          'left',
          choice(
            choice($.reserved_identifier_, $.identifier),
            $._destructuring_pattern
          )
        ),
        '=',
        field('right', $.expression)
      ),

    array_element: $ => choice($.expression, $.spread_element),

    array: $ =>
      seq(
        '[',
        optional_with_placeholder(
          'list',
          commaSep(optional(alias($.array_element, $.list_element)))
        ),
        ']'
      ),

    array_pattern: $ =>
      seq(
        '[',
        optional_with_placeholder(
          'array_pattern_list',
          commaSep(
            optional(alias($.array_pattern_element, $.pattern_list_element))
          )
        ),
        ']'
      ),

    array_pattern_element: $ => choice($.pattern, $.assignment_pattern),

    _jsx_element: $ => choice($.markup_element, $.markup_singleton_tag),

    markup_element: $ =>
      seq(
        field('open_tag', $.markup_opening_tag),
        optional_with_placeholder(
          'markup_content_list',
          repeat(alias($.jsx_child, $.markup_content))
        ),
        field('close_tag', $.markup_closing_tag)
      ),

    jsx_fragment: $ =>
      seq(
        field('markup_opening_tag', seq('<', '>')),
        optional_with_placeholder(
          'markup_content_list',
          repeat(alias($.jsx_child, $.markup_content))
        ),
        field('markup_closing_tag', seq('<', '/', '>'))
      ),

    jsx_text: $ => /[^{}<>]+/,

    jsx_embedded_expression_block: $ =>
      seq(
        '{',
        optional(
          field(
            'jsx_embedded_expression',
            choice($.expression, $.sequence_expression, $.spread_element)
          )
        ),
        '}'
      ),

    jsx_child: $ =>
      choice(
        $.jsx_text,
        $._jsx_element,
        $.jsx_fragment,
        alias($.jsx_embedded_expression_block, $.enclosed_body)
      ),

    markup_opening_tag: $ =>
      prec.dynamic(
        -1,
        seq(
          '<',
          field('identifier', $._jsx_element_name),
          optional_with_placeholder(
            'markup_attribute_list',
            repeat($._jsx_attribute)
          ),
          '>'
        )
      ),

    jsx_identifier: $ => /[a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]*/,

    jsx_identifier_: $ =>
      field('identifier', choice($.jsx_identifier, $.identifier)),

    nested_identifier: $ =>
      prec(
        'member',
        seq(
          field('identifier', choice($.identifier, $.nested_identifier)),
          '.',
          $.identifier
        )
      ),

    jsx_namespace_name: $ => seq($.jsx_identifier_, ':', $.jsx_identifier_),

    _jsx_element_name: $ =>
      choice($.jsx_identifier_, $.nested_identifier, $.jsx_namespace_name),

    markup_closing_tag: $ =>
      seq('<', '/', field('identifier', $._jsx_element_name), '>'),

    markup_singleton_tag: $ =>
      seq(
        '<',
        field('identifier', $._jsx_element_name),
        optional_with_placeholder(
          'markup_attribute_list',
          repeat($._jsx_attribute)
        ),
        '/',
        '>'
      ),

    _jsx_attribute: $ =>
      choice(
        $.markup_attribute,
        alias($.jsx_embedded_expression_block, $.enclosed_body)
      ),

    _jsx_attribute_name: $ =>
      field(
        'markup_attribute_name',
        choice($.jsx_identifier_, $.jsx_namespace_name)
      ),

    markup_attribute: $ =>
      seq($._jsx_attribute_name, optional(seq('=', $._jsx_attribute_value))),

    _jsx_attribute_value: $ =>
      field(
        'markup_attribute_value',
        choice(
          $.string,
          alias($.jsx_embedded_expression_block, $.enclosed_body),
          $._jsx_element,
          $.jsx_fragment
        )
      ),

    // Note that typescript overrides usage of class_heritage and the class/declaration constructs directly.
    class: $ =>
      prec(
        'literal',
        seq(
          optional_with_placeholder('decorator_list', repeat($.decorator)),
          'class',
          field('name', optional($.identifier)),
          optional($.class_heritage),
          field('body', $.class_body)
        )
      ),

    class_declaration: $ =>
      prec(
        'declaration',
        seq(
          optional_with_placeholder('decorator_list', repeat($.decorator)),
          'class',
          field('name', $.identifier),
          optional($.class_heritage),
          field('body', $.class_body),
          optional($._automatic_semicolon)
        )
      ),

    class_heritage: $ => seq('extends', $.expression),

    function: $ =>
      prec(
        'literal',
        seq(
          optional_with_placeholder('modifier_list', $.async_modifier),
          'function',
          optional($.identifier),
          $._call_signature,
          field('body', $.enclosed_body)
        )
      ),

    function_declaration: $ =>
      prec.right(
        'declaration',
        seq(
          optional_with_placeholder('modifier_list', $.async_modifier),
          'function',
          field('name', $.identifier),
          $._call_signature,
          field('body', $.enclosed_body),
          optional($._automatic_semicolon)
        )
      ),

    generator_function: $ =>
      prec(
        'literal',
        seq(
          optional_with_placeholder('modifier_list', $.async_modifier),
          'function',
          '*',
          optional($.identifier),
          $._call_signature,
          field('body', $.enclosed_body)
        )
      ),

    generator_function_declaration: $ =>
      prec.right(
        'declaration',
        seq(
          optional_with_placeholder('modifier_list', $.async_modifier),
          'function',
          '*',
          field('name', $.identifier),
          $._call_signature,
          field('body', $.enclosed_body),
          optional($._automatic_semicolon)
        )
      ),

    lambda: $ =>
      seq(
        optional_with_placeholder('modifier_list', $.async_modifier),
        choice(
          field('parameter', choice($.reserved_identifier_, $.identifier)),
          $.lambda_call_signature
        ),
        '=>',
        choice(field('return_value', $.expression), $.enclosed_body)
      ),

    lambda_call_signature: $ =>
      field('parameter_list_optional', $.formal_parameters),

    // Override
    _call_signature: $ => field('parameters', $.formal_parameters),
    parameter: $ => choice($.pattern, $.assignment_pattern),

    call: $ =>
      choice(
        prec(
          'call',
          seq(
            field('identifier', $.expression),
            field('arguments_', choice($.arguments, $.template_string))
          )
        ),
        prec(
          'member',
          seq(
            field('identifier', $.primary_expression),
            '?.',
            field('arguments_', $.arguments)
          )
        )
      ),

    new_expression: $ =>
      prec.right(
        'new',
        seq(
          'new',
          field('constructor', $.primary_expression),
          optional(prec.dynamic(1, $.arguments))
        )
      ),

    await_expression: $ => seq('await', $.expression),

    member_expression: $ =>
      prec(
        'member',
        seq(
          choice($.expression, $.primary_expression),
          choice('.', '?.'),
          choice($.private_property_identifier, $.identifier)
        )
      ),

    subscript_expression: $ =>
      prec.right(
        'member',
        seq(
          field('object', choice($.expression, $.primary_expression)),
          optional('?.'),
          '[',
          field('index', $._expressions),
          ']'
        )
      ),

    identifier_or_reserved: $ =>
      field('identifier', choice($._identifier, $.reserved_identifier_)),

    _lhs_expression: $ =>
      choice(
        $.member_expression,
        $.subscript_expression,
        $.identifier_or_reserved,
        $._destructuring_pattern
      ),

    assignment: $ =>
      prec.right(
        'assign',
        seq(
          field(
            'assignment_variable',
            choice($.parenthesized_expression, $._lhs_expression)
          ),
          '=',
          field('assignment_value', $.expression)
        )
      ),

    augmented_assignment_lhs: $ =>
      choice(
        $.member_expression,
        $.subscript_expression,
        field('identifier', choice($.reserved_identifier_, $.identifier)),
        $.parenthesized_expression
      ),

    augmented_assignment_expression: $ =>
      prec.right(
        'assign',
        seq(
          field('left', $.augmented_assignment_lhs),
          choice(
            '+=',
            '-=',
            '*=',
            '/=',
            '%=',
            '^=',
            '&=',
            '|=',
            '>>=',
            '>>>=',
            '<<=',
            '**=',
            '&&=',
            '||=',
            '??='
          ),
          field('right', $.expression)
        )
      ),

    assignment_initializer: $ =>
      seq('=', field('assignment_value', $.expression)),

    _destructuring_pattern: $ => choice($.object_pattern, $.array_pattern),

    spread_element: $ => seq('...', $.expression),

    ternary_expression: $ =>
      prec.right(
        'ternary',
        seq(
          field('condition', $.expression),
          '?',
          field('consequence', $.expression),
          ':',
          field('alternative', $.expression)
        )
      ),

    binary_expression: $ =>
      choice(
        ...[
          ['&&', 'binary_and'],
          ['||', 'binary_or'],
          ['>>', 'binary_times'],
          ['>>>', 'binary_times'],
          ['<<', 'binary_times'],
          ['&', 'binary_and'],
          ['^', 'binary_or'],
          ['|', 'binary_or'],
          ['+', 'binary_plus'],
          ['-', 'binary_plus'],
          ['*', 'binary_times'],
          ['/', 'binary_times'],
          ['%', 'binary_times'],
          ['**', 'binary_exp'],
          ['<', 'binary_relation'],
          ['<=', 'binary_relation'],
          ['==', 'binary_relation'],
          ['===', 'binary_relation'],
          ['!=', 'binary_relation'],
          ['!==', 'binary_relation'],
          ['>=', 'binary_relation'],
          ['>', 'binary_relation'],
          ['??', 'ternary'],
          ['instanceof', 'binary_relation'],
          ['in', 'binary_in'],
        ].map(([operator, precedence]) =>
          prec.left(
            precedence,
            seq(
              field('left', $.expression),
              field('operator', operator),
              field('right', $.expression)
            )
          )
        )
      ),

    unary_expression: $ =>
      choice(
        ...[
          ['!', 'unary_not'],
          ['~', 'unary_not'],
          ['-', 'unary_not'],
          ['+', 'unary_not'],
          ['typeof', 'unary_void'],
          ['void', 'unary_void'],
          ['delete', 'unary_void'],
        ].map(([operator, precedence]) =>
          prec.left(
            precedence,
            seq(field('operator', operator), field('argument_', $.expression))
          )
        )
      ),

    update_expression: $ =>
      prec.left(
        choice(
          seq(
            field('argument_', $.expression),
            field('operator', choice('++', '--'))
          ),
          seq(
            field('operator', choice('++', '--')),
            field('argument_', $.expression)
          )
        )
      ),

    sequence_expression: $ =>
      seq(
        field('left', $.expression),
        ',',
        field('right', choice($.sequence_expression, $.expression))
      ),

    //
    // Primitives
    //

    // Here we tolerate unescaped newlines in double-quoted and
    // single-quoted string literals.
    // This is legal in typescript as jsx/tsx attribute values (as of
    // 2020), and perhaps will be valid in javascript as well in the
    // future.
    //
    string: $ =>
      choice(
        seq(
          '"',
          repeat(
            choice(
              alias($.unescaped_double_string_fragment, $.string_fragment),
              $.escape_sequence
            )
          ),
          '"'
        ),
        seq(
          "'",
          repeat(
            choice(
              alias($.unescaped_single_string_fragment, $.string_fragment),
              $.escape_sequence
            )
          ),
          "'"
        )
      ),

    // Workaround to https://github.com/tree-sitter/tree-sitter/issues/1156
    // We give names to the token() constructs containing a regexp
    // so as to obtain a node in the CST.
    //
    unescaped_double_string_fragment: $ => token.immediate(prec(1, /[^"\\]+/)),

    // same here
    unescaped_single_string_fragment: $ => token.immediate(prec(1, /[^'\\]+/)),

    escape_sequence: $ =>
      token.immediate(
        seq(
          '\\',
          choice(
            /[^xu0-7]/,
            /[0-7]{1,3}/,
            /x[0-9a-fA-F]{2}/,
            /u[0-9a-fA-F]{4}/,
            /u{[0-9a-fA-F]+}/
          )
        )
      ),

    // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    comment: $ =>
      token(
        choice(seq('//', /.*/), seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/'))
      ),

    template_string: $ =>
      seq(
        '`',
        repeat(
          choice($._template_chars, $.escape_sequence, $.template_substitution)
        ),
        '`'
      ),

    template_substitution: $ => seq('${', $._expressions, '}'),

    regex: $ =>
      seq(
        '/',
        field('pattern', $.regex_pattern),
        token.immediate('/'),
        optional(field('flags', $.regex_flags))
      ),

    regex_pattern: $ =>
      token.immediate(
        prec(
          -1,
          repeat1(
            choice(
              seq(
                '[',
                repeat(
                  choice(
                    seq('\\', /./), // escaped character
                    /[^\]\n\\]/ // any character besides ']' or '\n'
                  )
                ),
                ']'
              ), // square-bracket-delimited character class
              seq('\\', /./), // escaped character
              /[^/\\\[\n]/ // any character besides '[', '\', '/', '\n'
            )
          )
        )
      ),

    regex_flags: $ => token.immediate(/[a-z]+/),

    number: $ => {
      const hex_literal = seq(choice('0x', '0X'), /[\da-fA-F](_?[\da-fA-F])*/)

      const decimal_digits = /\d(_?\d)*/
      const signed_integer = seq(optional(choice('-', '+')), decimal_digits)
      const exponent_part = seq(choice('e', 'E'), signed_integer)

      const binary_literal = seq(choice('0b', '0B'), /[0-1](_?[0-1])*/)

      const octal_literal = seq(choice('0o', '0O'), /[0-7](_?[0-7])*/)

      const bigint_literal = seq(
        choice(hex_literal, binary_literal, octal_literal, decimal_digits),
        'n'
      )

      const decimal_integer_literal = choice(
        '0',
        seq(
          optional('0'),
          /[1-9]/,
          optional(seq(optional('_'), decimal_digits))
        )
      )

      const decimal_literal = choice(
        seq(
          decimal_integer_literal,
          '.',
          optional(decimal_digits),
          optional(exponent_part)
        ),
        seq('.', decimal_digits, optional(exponent_part)),
        seq(decimal_integer_literal, exponent_part),
        seq(decimal_digits)
      )

      return token(
        choice(
          hex_literal,
          decimal_literal,
          binary_literal,
          octal_literal,
          bigint_literal
        )
      )
    },

    // 'undefined' is syntactically a regular identifier in JavaScript.
    // However, its main use is as the read-only global variable whose
    // value is [undefined], for which there's no literal representation
    // unlike 'null'. We gave it its own rule so it's easy to
    // highlight in text editors and other applications.
    _identifier: $ => choice($.undefined, $.identifier),

    identifier: $ => {
      const alpha =
        /[^\x00-\x1F\s0-9:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B\u00A0]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\}/
      const alphanumeric =
        /[^\x00-\x1F\s:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B\u00A0]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\}/
      return token(seq(alpha, repeat(alphanumeric)))
    },

    private_property_identifier: $ => {
      const alpha =
        /[^\x00-\x1F\s0-9:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B\u00A0]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\}/
      const alphanumeric =
        /[^\x00-\x1F\s:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B\u00A0]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\}/
      return token(seq('#', alpha, repeat(alphanumeric)))
    },

    meta_property: $ => seq('new', '.', 'target'),

    this: $ => 'this',
    super: $ => 'super',
    true: $ => 'true',
    false: $ => 'false',
    null: $ => 'null',
    undefined: $ => 'undefined',

    //
    // Expression components
    //

    argument: $ => choice($.expression, $.spread_element),

    arguments: $ =>
      seq(
        '(',
        optional_with_placeholder(
          'argument_list',
          commaSep(optional($.argument))
        ),
        ')'
      ),

    decorator: $ =>
      seq(
        '@',
        field(
          'decorator_expression',
          choice(
            $.identifier,
            alias($.decorator_member_expression, $.member_expression),
            alias($.decorator_call_expression, $.call)
          )
        )
      ),

    decorator_member_expression: $ =>
      prec(
        'member',
        seq(
          field(
            'object',
            choice(
              $.identifier,
              alias($.decorator_member_expression, $.member_expression)
            )
          ),
          '.',
          field('property_', alias($.identifier, $.property_identifier))
        )
      ),

    decorator_call_expression: $ =>
      prec(
        'call',
        seq(
          field(
            'function_',
            choice(
              $.identifier,
              alias($.decorator_member_expression, $.member_expression)
            )
          ),
          field('arguments_', $.arguments)
        )
      ),

    class_body: $ =>
      seq(
        '{',
        optional_with_placeholder(
          'class_member_list',
          repeat(alias($.class_member, $.member))
        ),
        '}'
      ),

    class_member: $ =>
      choice(
        seq(field('member_', $.method_definition), optional(';')),
        seq(field('property', $.field_definition), $._semicolon)
      ),

    field_definition: $ =>
      seq(
        optional_with_placeholder('decorator_list', repeat($.decorator)),
        optional_with_placeholder('modifier_list', $.static_modifier),
        field('assignment_list', alias($.field_assignment, $.assignment))
      ),

    field_assignment: $ =>
      seq(
        field('assignment_variable', $.property_name),
        optional_with_placeholder(
          'assignment_value_list_optional',
          $.assignment_initializer
        )
      ),

    formal_parameters: $ =>
      seq(
        '(',
        optional_with_placeholder(
          'parameter_list',
          seq(commaSep1($.parameter), optional(','))
        ),
        ')'
      ),

    // This negative dynamic precedence ensures that during error recovery,
    // unfinished constructs are generally treated as literal expressions,
    // not patterns.
    pattern: $ =>
      prec.dynamic(
        -1,
        choice(
          $.identifier,
          alias($.reserved_identifier_, $.identifier_),
          $._destructuring_pattern,
          $.rest_pattern
        )
      ),

    rest_pattern: $ =>
      seq('...', choice($.identifier, $._destructuring_pattern)),

    method_definition: $ =>
      seq(
        optional_with_placeholder('decorator_list', repeat($.decorator)),
        optional_with_placeholder(
          'modifier_list',
          seq(
            optional($.static_modifier),
            optional($.async_modifier),
            optional($.accessors_modifier)
          )
        ),
        field('name', $.property_name),
        field('parameters', $.formal_parameters),
        field('body', $.enclosed_body)
      ),

    key_value_pair: $ =>
      seq(
        field('key_value_pair_key', $.property_name),
        ':',
        field('key_value_pair_value', $.expression)
      ),

    pair_pattern: $ =>
      seq(
        field('key_value_pair_key', $.property_name),
        ':',
        field('key_value_pair_value', $.pattern)
      ),

    property_name: $ =>
      field(
        'identifier',
        choice(
          $.identifier,
          $.reserved_identifier_,
          $.private_property_identifier,
          $.string,
          $.number,
          $.computed_property_name
        )
      ),

    computed_property_name: $ => seq('[', $.expression, ']'),

    reserved_identifier_: $ =>
      choice('get', 'set', 'async', 'static', 'export'),

    _semicolon: $ => choice($._automatic_semicolon, ';'),
  },
})

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)))
}

function commaSep(rule) {
  return optional(commaSep1(rule))
}

function optional_with_placeholder(field_name, rule) {
  return choice(field(field_name, rule), field(field_name, blank()))
}
