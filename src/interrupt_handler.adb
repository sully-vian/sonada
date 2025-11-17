with Ada.Text_IO;          use Ada.Text_IO;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with Ada.Interrupts;

package body Interrupt_Handler is
    Show_Cursor : constant String := Character'Val (27) & "[?25h";
    Reset       : constant String := Character'Val (27) & "[0m";

    protected INT_Handler is
        procedure ISR;
        pragma Interrupt_Handler (ISR);
        pragma Attach_Handler (ISR, SIGINT);
    end INT_Handler;

    protected body INT_Handler is
        procedure ISR is
        begin
            Put (Show_Cursor & Reset);
            New_Line;
            Put_Line ("Interrupted.");
            OS_Exit (0);
        end;
    end INT_Handler;

    INT_Dummy : constant Boolean := INT_Handler'Elaborated;
end Interrupt_Handler;
