

psrc_theme <- bs_theme(bg = "white",
                       fg = "black",
                       base_font = font_google("Poppins", local = TRUE),
                       primary = "#00716C",
                       version = 5) |>
  
                       bs_add_rules("
                       
                       /* NavBar menu items */
                       .nav-link.active {
                       border-bottom-color: #EC9B21 !important;
                       border-bottom-width: 2.5px !important;
                       font-weight: 600 !important;
                       font-size: 1.25rem;
                       }
                       
                       .nav-link {
                       font-size: 1.25rem;
                       }
                      
                       .nav-link:hover {
                       border-bottom-color: #00716C !important;
                       border-bottom-width: 2.5px !important;
                       font-size: 1.25rem;
                       }
                       
                       /* General Text settings */
                       h2 {
                       font-size: 1.625rem;
                       font-weight: 600;
                       margin-bottom: 1.5rem;
                       margin-top: 0;
                       line-height: 2.5rem;
                       }
                       
                       /* accordion settings */
                       
                       .accordion-button {
                       padding: 0rem 0rem !important;
                       border: none !important;
                       border-color: white !important;
                       }
                       
                       .accordion-item {
                       padding: 0rem 0rem !important;
                       border: none !important;
                       border-color: white !important;
                       }
                       
                       .accordion-title {
                       color: #00716C;
                       text-decoration-line: underline;
                       text-decoration-color: #00716C;
                       text-decoration-thickness: 1.5px;
                       font-size: 12pt;
                       font-weight: 600;
                       padding-left: 0rem;
                       padding-top: 0rem;
                       padding-bottom: 0rem;
                       padding-right: 0rem;
                       }

                       .source_url {
                       color: #00716C;
                       font-size: 12pt;
                       }
                       
                       .left-panel-url {
                       font-weight: 600;
                       }
                       
                       .m-menu__title {
                       font-size: 22px;
                       font-weight: 400;
                       margin-bottom: 0.5rem;
                       color: #2F3030;
                       }
                       
                       /* footer styles */
                       
                       .responsive-image {
                       width: 100%;
                       }
                       
                       @media (max-width: 768px) {
                       .responsive-image {
                       width: 50%;
                       }
                       }
                       
                       .footer_first_row {
                       padding-bottom: 0rem;
                       }
                       
                       .footer_panel {
                       background-color: #4A0048;
                       padding-left: 1rem;
                       padding-top: 1rem;
                       padding-bottom: 1rem;
                       padding-right: 1rem;
                       }
                       
                       .footer_feedback {
                       color: white;
                       font-size: 1.25rem;
                       font-weight: 400;
                       line-height: 1.25rem;
                       }
                       
                       .footer_heading {
                       color: white; 
                       font-size: 1.25rem;
                       font-weight: 600;
                       padding-bottom: 1.5rem;
                       }
                       
                       .footer_about {
                       color: white; 
                       font-size: 1rem;
                       font-weight: 200;
                       }
                       
                       .psrc-location {
                       color: white; 
                       font-size: 1rem;
                       font-weight: 200;
                       padding-bottom: 0.75rem;
                       }
                       
                       .psrc-phone {
                       color: white; 
                       font-size: 1rem;
                       font-weight: 200;
                       padding-bottom: 0.75rem;
                       }
                       
                       .psrc-email {
                       color: #EC9B21; 
                       font-size: 1rem;
                       font-weight: 200;
                       padding-bottom: 0.75rem;
                       }
                       
                       .footer_footer {
                       background-color: #630460;
                       padding-left: 0rem;
                       padding-top: 1rem;
                       padding-bottom: 0rem;
                       padding-right: 0rem;
                       color: #EC9B21;
                       }
                       
                       .footer_url {
                       color: #EC9B21;
                       font-size: 12pt;
                       }
                       
                       i {
                       color: #EC9B21;
                       font-size: 14pt;
                       padding-left: 15px;
                       padding-right: 15px;
                       }
                       
                       .insights_panel {
                       background-color: #C0E095;
                       clip-path: polygon(0 0, 100% 0, 100% 20%, 100% 80%, 95% 100%, 20% 100%, 0 100%, 0% 20%);
                       padding-left: 1.5rem;
                       padding-top: 1.5rem;
                       padding-bottom: 1.5rem;
                       padding-right: 1.5rem;
                       }
                       
                       .value-box-outcomes .value-box-value {
                       order: 1 !important;
                       font-size: 2rem !important;
                       font-weight: 600 !important;
                       }
                       
                       .value-box-outcomes .value-box-title {
                       color: #2F3030 !important;
                       order: 2 !important;
                       font-size: 1rem !important;
                       margin-top: 0 !important;
                       line-height: 1.5 !important;
                       font-weight: 200 !important;
                       }
                       

                       ")
