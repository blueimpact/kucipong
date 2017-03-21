const path              = require('path');
const webpack           = require('webpack');
const merge             = require('webpack-merge');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

console.log('Start Webpack process...');

// Determine build env by npm command options
const TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? 'production' : 'development';
const ENV = {
  'googleMapApiKey':
    process.env.GOOGLE_MAP_API_KEY ||
      // This API key is only available on `stage.kucipong.com`.
      'AIzaSyA6yBv38YNHWzaAI5S7c27JfqkSFMFC-7g',
};

// Common webpack config
const commonConfig = {

  output: {
    path: path.resolve(__dirname, 'dist/'),
    filename: '/static/[name].js',
  },

  entry: {
    chat: [
      path.join( __dirname, 'src/chat.js' )
    ],
    endUser: [
      path.join( __dirname, 'src/endUser.js' )
    ],
    storeUser: [
      path.join( __dirname, 'src/storeUser.js' )
    ],
    adminUser: [
      path.join( __dirname, 'src/adminUser.js' )
    ],
    "storeUser_store_edit": [
      path.join( __dirname, 'src/js/storeUser_store_edit.js' )
    ],
    "storeUser_store_coupon_id_edit": [
      path.join( __dirname, 'src/js/storeUser_store_coupon_id_edit.js' )
    ],
  },

  resolve: {
    extensions: ['.js', '.elm'],
    modules: [
      'node_modules',
      path.resolve('src/elm'),
    ],
  },

  module: {
    rules: [
      {
        test: /\.(eot|ttf|woff|woff2|svg)$/,
        use: [
          {
            loader: 'file-loader',
            options: {
              name: "/static/[name]-[hash].[ext]",
            },
          },
        ]
      },
      {
        test: /\.pug$/,
        use: 'pug-loader',
      },
      {
        test: /\.(jpg|jpeg|png)$/,
        use: 'url-loader'
      },
    ]
  },

  plugins: [
      // Chat page
      {
        chunks: ['chat'],
        template: 'chat',
        outputDir: 'static',
      },
    ].concat(
      [
        // Pages for end users.
        'endUser_category',
        'endUser_coupon_id',
        'endUser_store_id',
        'endUser_store_id_coupon',
      ].map((template) => ({
        chunks: ['endUser'],
        template
      }))
    ).concat(
      // Pages for store users.
      [
        'storeUser_login',
        'storeUser_store',
        'storeUser_store_edit',
        'storeUser_store_coupon',
        'storeUser_store_coupon_id',
        'storeUser_store_coupon_id_edit',
      ].map((template) => ({
        chunks: ['storeUser'],
        template
      }))
    ).concat(
      // Pages for admin users
      [
        'adminUser_login',
        'adminUser_admin_store_create',
        'adminUser_admin_store_delete',
        'adminUser_admin_store_delete_confirm',
        'adminUser_admin_store_login',
      ].map((template) => ({
        chunks: ['adminUser'],
        template
      }))
    ).map((o) =>
      new HtmlWebpackPlugin({
        chunks: o.chunks,
        template: `${o.directory || 'src/pug'}/${o.template}.pug`,
        inject:   'body',
        filename: `${o.outputDir || '.'}/${o.template}.html`,
        data: ENV,
        hash: true,
      })
    ).concat(
      [
        // Inject variables to JS file.
        new webpack.DefinePlugin({
          'process.env':
            Object.keys(ENV).reduce((o, k) =>
              merge(o, {
                [k]: JSON.stringify(ENV[k]),
              }), {}
            ),
        }),
      ]
    ),
};

// Settings for `npm start`
if (TARGET_ENV === 'development') {
  console.log('Serving locally...');

  module.exports = merge(commonConfig, {

    devtool: 'source-map',
    devServer: {
      contentBase: 'src',
      inline: true,
      port: ENV.port,
      host: ENV.host,
    },

    module: {
      rules: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            {
              loader: 'elm-hot-loader',
            },
            { loader: 'elm-webpack-loader',
              options: {
                verbose: true,
                warn: true,
              },
            },
          ],
        },
        {
          test: /\.(css|scss)$/,
          use: [
            'style-loader',
            'css-loader',
            'resolve-url-loader',
            'sass-loader',
            'postcss-loader',
          ],
        },
      ],
    },
  });
}

// Settings for `npm run build`.
if (TARGET_ENV === 'production') {
  console.log('Building for prod...');

  module.exports = merge(commonConfig, {

    module: {
      rules: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader:  'elm-webpack-loader',
        },
        {
          test: /\.(css|scss)$/,
          use: ExtractTextPlugin.extract({
            fallback: 'style-loader',
            use: [
              'css-loader',
              'resolve-url-loader',
              'sass-loader',
              'postcss-loader',
            ],
          }),
        },
      ],
    },

    plugins: [
      new CopyWebpackPlugin([
        // {
        //   from: 'src/favicon.ico'
        // },
      ]),

      // Extract CSS into a separate file
      new ExtractTextPlugin({
        filename: '/static/[name].css', disable: false, allChunks: true
      }),

      // Minify & mangle JS/CSS
      new webpack.optimize.UglifyJsPlugin({
          minimize:   true,
          compressor: { warnings: false }
      }),
    ],
  });
}
