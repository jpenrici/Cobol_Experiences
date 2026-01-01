#include <algorithm>
#include <libcob.h>
#include <print>
#include <string>

struct EmployeeData {
  uint16_t id;        // PIC 9(04)    USAGE COMP-5
  char name[20];      // PIC X(20)
  uint32_t hours;     // PIC 9(03)V99 USAGE COMP-5
  uint32_t rate;      // PIC 9(03)V99 USAGE COMP-5
  uint64_t gross_pay; // PIC 9(07)V99 USAGE COMP-5

  // Helper for setting the name with automatic padding.
  void set_name(std::string_view new_name) {
    // Complete filling with spaces
    std::fill(std::begin(name), std::end(name), ' ');
    auto size = std::min(new_name.size(), sizeof(name));
    std::copy_n(new_name.data(), size, name);
  }
  // Ensure the struct is packed to match COBOL's memory layout
} __attribute__((packed));

// Prototype for the COBOL function
extern "C" {
// Matches PROGRAM-ID in payroll.cbl
void PAYROLL_ENGINE(EmployeeData *data);
}

auto main(int argc, char **argv) -> int {

  // Initializes the GnuCOBOL runtime environment
  cob_init(argc, argv);

  EmployeeData emp{};
  emp.id = 101;
  emp.set_name("Cpp Cobol");
  emp.hours = 4000; // 40.00 (Fixed point)
  emp.rate = 5050;  // 50.50 (Fixed point)

  std::println("C++: Sending data to COBOL...");

  // Call COBOL subroutine
  PAYROLL_ENGINE(&emp);

  std::println("-----------------------------------");
  std::println("C++: Results from COBOL Engine:");
  std::println("ID: {}", static_cast<int>(emp.id));
  std::println("Name: {}", std::string(emp.name, 20));
  std::println("Gross Pay: {:.2f}", static_cast<double>(emp.gross_pay) / 100.0);
  std::println("-----------------------------------");

  return 0;
}
